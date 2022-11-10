import math
from rl_server.so_routing.env.toy_driver_env.driver import Driver, DriverState
from rl_server.so_routing.env.toy_driver_env.efraimidis_spirakis_sampling import reservoir_sampling
from rl_server.so_routing.env.toy_driver_env.reward import jain_user_fairness
from ray.rllib.env.env_context import EnvContext
from ray.rllib.env.multi_agent_env import MultiAgentEnv
import random
from typing import Dict, List, Tuple
import logging
import json


log = logging.getLogger(__name__)


class ToyDriverEnv(MultiAgentEnv):

    def __init__(
        self,
        config: EnvContext
    ):

        self.n_drivers = config['n_drivers']
        self.n_auctions = config['n_auctions']
        self.max_balance = config['max_balance']
        self.max_trip_duration = config['max_trip_duration']
        self.min_start_time = config['min_start_time']
        self.max_start_time = config['max_start_time']
        self.max_batch_size = config['max_batch_size']
        self.max_replannings = config['max_replannings']
        self.delay_increment = config['delay_increment']
        self.timestep_size = config['timestep_size']
        self.dist_per_timestep = config['dist_per_timestep']
        # Callable[[Driver], List[float]]
        self.observation_fn = config['observation_fn']
        # Callable[[List[Driver]], int]
        self.network_signal_fn = config['network_signal_fn']
        self.balance_initialization = config.get(
            'balance_initialization', 'uniform')
        self.trip_initialization = config.get('trip_initialization', 'uniform')
        self.action_space = config['action_space']
        self.observation_space = config['observation_space']

    def step(self, action):
        """Returns observations from ready agents.

        The returns are dicts mapping from agent_id strings to values. The
        number of agents in the env can vary over time.

        Returns:
            Tuple containing 1) new observations for
            each ready agent, 2) reward values for each ready agent. If
            the episode is just started, the value will be None.
            3) Done values for each ready agent. The special key
            "__all__" (required) is used to indicate env termination.
            4) Optional info values for each agent id.

        Examples:
            >>> env = ...
            >>> obs, rewards, dones, infos = env.step(
            ...    action_dict={
            ...        "car_0": 1, "car_1": 0, "traffic_light_1": 2,
            ...    })
            >>> print(rewards)
            {
                "car_0": 3,
                "car_1": -1,
                "traffic_light_1": 0,
            }
            >>> print(dones)
            {
                "car_0": False,    # car_0 is still running
                "car_1": True,     # car_1 is done
                "__all__": False,  # the env is not done
            }
            >>> print(infos)
            {
                "car_0": {},  # info for car_0
                "car_1": {},  # info for car_1
            }
        """

        # (t) - update routes and step forward in time
        self.resolve_auctions(action)
        moved = self.move_active_trips()
        self.current_time += self.timestep_size
        self.activate_new_trips()
        report = self.report_state()
        print(f"TIME: {self.current_time}")
        print(json.dumps(report, indent=4))
        # print(f'moved {moved} trips')

        # (t+1) - observe state
        done = self.generate_dones()  # side-effect: advance driver state
        rew = self.generate_rewards()
        obs = self.generate_observations()

        # (t+1) - set up auctions to resolve in (t+2)
        self.clear_auctions()
        self.create_auctions()
        # n_auctions, pct_bidding = self.create_auctions()

        # print(f'{n_auctions} auctions ({pct_bidding*100:.2f}% of active)')
        # if self.done():
        #     print("DONE")
        #     for d in sorted(self.drivers, key=lambda d: d.trip_start_time):
        #         print(d)
        return obs, rew, done, {}

    def reset(self):
        """Resets the env and returns observations from ready agents.

        Returns:
            New observations for each ready agent.

        Examples:
            >>> from ray.rllib.env.multi_agent_env import MultiAgentEnv
            >>> class MyMultiAgentEnv(MultiAgentEnv): 
            ...     # Define your env here. 
            ...     ... 
            >>> env = MyMultiAgentEnv() 
            >>> obs = env.reset() 
            >>> print(obs) 
            {
                "car_0": [2.4, 1.6],
                "car_1": [3.4, -3.2],
                "traffic_light_1": [0, 3, 5, 1],
            }
        """
        def init_balance():
            if self.balance_initialization == "uniform":
                return random.randint(0, self.max_balance)
            elif isinstance(self.balance_initialization, int):
                if self.balance_initialization > self.max_balance:
                    comparison = f"{self.balance_initialization} > {self.max_balance}"
                    raise Exception(
                        f"init balance > max balance: {comparison}")
                return self.balance_initialization
            else:
                msg = f"unknown balance initialization type {self.balance_initialization}"
                raise Exception(msg)

        def init_start():
            return random.randint(self.min_start_time, self.max_start_time)

        def init_trip():
            if self.trip_initialization == "uniform":
                return random.randint(1, self.max_trip_duration)
            elif isinstance(self.trip_initialization, int):
                return self.trip_initialization
            else:
                msg = f"unknown trip initialization type {self.trip_initialization}"
                raise Exception(msg)

        # create drivers and activate trips that start at time zero
        self.drivers: List[Driver] = [
            Driver(i, init_balance(), init_start(), init_trip())
            for i in range(self.n_drivers)]
        self.current_time = 0
        self.auctions: List[List[Driver]] = []
        self.auction_lookup: Dict[str, int] = []
        self.activate_new_trips()
        obs = self.generate_observations()

        print(f"starting new episode at time {self.current_time}")

        # for d in sorted(self.drivers, key=lambda d: d.trip_start_time):
        #     print(d)

        return obs

    def done(self):
        return all([d.done for d in self.drivers])

    def generate_dones(self):
        """
        reports on drivers who are done. when a driver arrives
        at their destination, we want to report they are done
        exactly once. to do this, we advance them from an "ARRIVED"
        state to a "DONE" state and add them to the Env's done 
        message. if all agents are "DONE", we can report that the
        episode is done ("__all__" = True).
        """
        done_msg = {}
        for driver in self.drivers:
            if driver.arrived:
                done_msg[driver.driver_id] = True
                self.drivers[driver.driver_id] = driver.report_done()
        done_msg['__all__'] = self.done()
        return done_msg

    def activate_new_trips(self):
        newly_activated = 0
        for driver in self.drivers:
            if driver.trip_start_time == self.current_time:
                updated = driver.start_trip(self.current_time)
                self.drivers[updated.driver_id] = updated
                newly_activated += 1
        return newly_activated

    def resolve_auctions(self, bids: Dict[str, int]):
        # for each auction, sample winners

        for a in self.auctions:
            auction_size = len(a)
            n_winners = self.network_signal_fn(a)
            n_losers = auction_size - n_winners
            if n_losers == 0:
                continue

            # grab bids for drivers in this auction
            auction_data = [(d.driver_id, bids.get(str(d.driver_id)))
                            for d in a]

            # protect against over-bidding
            for idx, (d_id, bid) in enumerate(auction_data):
                driver = self.drivers[d_id]
                if driver.balance <= bid:
                    auction_data[idx] = d_id, driver.balance
                    # msg = (
                    #     f'driver {d_id} overbid {bid} which exceeds '
                    #     f'balance of {driver.balance}'
                    # )
                    # print(msg)

            # print(f'auction bids (id, bid): {auction_data}')

            # use weighted sampling to pick winners from the bids
            a_ids, a_bids = zip(*auction_data)
            winner_ids = reservoir_sampling(a_ids, a_bids, n_winners, random)

            # assign winners/losers
            winners = [d for d in a if d.driver_id in winner_ids]
            win_bids = [bids.get(str(d.driver_id))
                        for d in a if d.driver_id in winner_ids]
            losers = [d.reroute(self.delay_increment, self.current_time)
                      for d in a if not d.driver_id in winners]

            # distribute payments for winner-pay-all auction
            overflow_pool = 0
            for i in range(len(winners)):
                for j in range(len(losers)):
                    # pull latest state for winner/loser
                    winner = winners[i]
                    win_bid = win_bids[i]
                    loser = losers[i]

                    # payment p due from d1 to d2, a fraction of d1's bid
                    # proportional to the number of losers to distribute to
                    p = int(math.floor(float(win_bid) / float(len(losers))))
                    exceeds_winner_balance = p > winner.balance
                    exceeds_loser_max = loser.balance + p > self.max_balance
                    if p == 0 or exceeds_winner_balance or exceeds_loser_max:
                        continue
                    winner_updated, winner_overs = winner.increment_balance(
                        -p, self.max_balance)
                    loser_updated, loser_overs = loser.increment_balance(
                        p, self.max_balance)
                    overflow_pool += (winner_overs + loser_overs)
                    winners[i] = winner_updated
                    losers[j] = loser_updated

            # we have an overflow pool because the currency is bounded.
            # redistribute any funds from the overflow pool, using a round robin
            # assignment of a single currency unit to the bidders until the pool
            # is exhausted. if all agents reach their limit before the pool
            # is emptied, raise an error
            def redistributing():
                return overflow_pool > 0
            i = 0
            # print(f'overflow pool: {overflow_pool}')
            overflow_drivers = winners.copy()
            overflow_drivers.extend(losers)
            while redistributing():
                driver = overflow_drivers[i]
                updated, overs = driver.increment_balance(1, self.max_balance)
                if overs == 0:
                    overflow_pool -= 1
                    overflow_drivers[i] = updated
                no_headroom = all(
                    [d.balance == self.max_balance for d in overflow_drivers])
                if redistributing() and no_headroom:
                    msg = (
                        f"overflow pool has {overflow_pool} remaining but all "
                        f"agents in this auction are maxxed on balance"
                    )
                    for d in sorted(a, key=lambda d: d.driver_id):
                        print(d)
                    for d, _ in sorted(overflow_drivers, key=lambda d: d.driver_id):
                        print(d)
                    raise Exception(msg)
                i = (i + 1) % len(overflow_drivers)

            # update the final driver states after settling the auction
            for updated_driver in overflow_drivers:
                self.drivers[updated_driver.driver_id] = updated_driver

    def move_active_trips(self):
        moved = 0
        for driver in self.drivers:
            if driver.active:
                updated = driver.move(self.dist_per_timestep)
                self.drivers[updated.driver_id] = updated
                moved += 1
                # print(
                #     f'time {self.current_time} - driver {driver.driver_id} moved and '
                #     f'has traversed {updated.trip_pct()*100:.2f}% of trip'
                # )
        return moved

    def create_auctions(self) -> Tuple[int, float]:
        active = [d for d in self.drivers
                  if d.active and not d.replannings >= self.max_replannings]
        random.shuffle(active)
        n_active = len(active)
        if n_active < 2:
            # print(f'no auctions to create, # of active trips is {n_active}')
            return 0, 0.0
        else:
            while len(active) > 1 and len(self.auctions) < self.n_auctions:
                batch_size = random.randint(2, self.max_batch_size)
                # append a batch
                if len(active) < batch_size:
                    batch = active
                    active = []
                else:
                    batch = [active.pop(0) for _ in range(batch_size)]

                # update auctions
                self.auctions.append(batch)
                auction_id = len(self.auctions) - 1
                ids = {d.driver_id: auction_id for d in batch}
                self.auction_lookup.update(ids)

            in_auction = len([d for a in self.auctions for d in a])
            auction_pct = in_auction / n_active
            n_auctions = len(self.auctions)
            return n_auctions, auction_pct

    def clear_auctions(self):
        self.auctions: List[List[Driver]] = []
        self.auction_lookup = {}

    def generate_observations(self):
        return {str(d.driver_id): self.observation_fn(d) for d in self.drivers if d.active}

    def generate_rewards(self):
        done = self.done()
        # print(f"generating rewards, done? {done}")
        if not done:
            r = {str(d.driver_id): 0.0 for a in self.auctions for d in a}
            return r
        else:
            allocations = [d.pct_original_of_final() for d in self.drivers]
            fairness = jain_user_fairness(allocations)
            rewards = {str(d.driver_id): f for d,
                       f in zip(self.drivers, fairness)}
            # print("FINAL REWARDS")
            # for d_id, r in rewards.items():
            #     pad_id = str(d_id).ljust(5)
            #     d = self.drivers[int(d_id)]
            #     print(f'{pad_id}: {r:.6f} {d}')
            return rewards

    def report_state(self):
        report = {s.name: 0 for s in DriverState}
        for d in self.drivers:
            report[d.state.name] = report[d.state.name] + 1
        return report
