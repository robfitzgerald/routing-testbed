import math
from rl_server.so_routing.env.toy_driver_env.driver import Driver, DriverState
from rl_server.so_routing.env.toy_driver_env.auction import resolve_winner_pay_all_auction
from rl_server.so_routing.env.toy_driver_env.efraimidis_spirakis_sampling import reservoir_sampling
from rl_server.so_routing.env.toy_driver_env.reward import jain_user_fairness
from ray.rllib.env.env_context import EnvContext
from ray.rllib.env.multi_agent_env import MultiAgentEnv
import random
from typing import Dict, List, Tuple
import logging
import json
import csv
import pandas as pd
import statistics as stats

from rl_server.so_routing.env.toy_driver_env.scenario.scenario import Scenario


log = logging.getLogger(__name__)


report_fieldnames = [
    'iteration',
    'n_drivers',
    'sum_initial_balance',
    'sum_final_balance',
    'mean_balance',
    'mean_balance_diff',
    'std_balance_diff',
    'mean_allocation',
    'std_allocation',
    'mean_allocation_replanned_only',
    'mean_reward',
    'mean_original_trip_distance',
    'mean_experienced_trip_distance',
    'mean_replannings',
    'mean_delay',
    'mean_delay_pct',
    'std_delay_pct',
    'mean_increase'
]


class ToyDriverEnv(MultiAgentEnv):

    def __init__(
        self,
        config: EnvContext
    ):
        # OPENAI GYM ARGUMENTS
        self.action_space = config['action_space']
        self.observation_space = config['observation_space']

        # ENVIRONMENT-SPECIFIC ARGUMENTS
        self.scenario: Scenario = config['scenario']
        self.max_balance = config['max_balance']
        self.min_start_time = config['min_start_time']
        self.max_start_time = config['max_start_time']
        self.max_replannings = config['max_replannings']
        self.max_trip_increase_pct = config['max_trip_increase_pct']
        self.timestep_size = 1
        # Callable[[int], int]  from iteration # to population size
        self.population_fn = config['population_fn']
        # Callable[[Driver, int], List[float]]
        self.observation_fn = config['observation_fn']
        # Callable[[List[Driver]], int]
        self.network_signal_fn = config['network_signal_fn']
        # Union[int,str]
        self.balance_initialization = config.get('balance_initialization', 'uniform')
        
        # input validation
        assert self.max_trip_increase_pct >= 0.0, "max_trip_increase_pct must be in [0,1]"
        assert self.max_trip_increase_pct <= 1.0, "max_trip_increase_pct must be in [0,1]"

        # set up optional logging
        log_filename = config.get('log_filename')
        if log_filename is not None:
            self.f = open(log_filename, 'w')
            self.report_logger = csv.DictWriter(self.f, fieldnames=report_fieldnames)
            self.report_logger.writeheader()
        else:
            self.report_logger = None

        # STATEFUL ARGUMENTS
        self.rng = random.Random()
        self.iteration = 0
        self.stats = {}

        log.info(f"loaded '{self.scenario.name}' scenario with configuration:")
        print(json.dumps(self.scenario.as_dict(), indent=4))

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
        self.move_active_trips()
        self.current_time += self.timestep_size
        self.activate_new_trips()
        # print(json.dumps(report, indent=4))
        # print(f'moved {moved} trips')

        # (t+1) - observe state
        done = self.generate_dones()  # side-effect: advance driver state
        rew, allocs = self.generate_rewards(done['__all__'])


        # (t+1) - set up auctions to resolve in (t+2), and generate observations
        #         for the drivers that are participating in auctions
        self.clear_auctions()
        self.create_auctions()
        obs = self.generate_observations()

        report = self.report_state()
        auctions = [len(a) for a in self.auctions]
        print(f"TIME: {str(self.current_time).ljust(3)} {report} AUCTIONS: {auctions}")

        if done['__all__']:
            self.append_final_stats(rew, allocs)
            self.log_report_row(self.stats)
            print("FINISHED")
            print(json.dumps(self.stats, indent=4))

        # info = self.stats if done['__all__'] else {}

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
        self.iteration = self.iteration + 1

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
            return self.scenario.sample_initial_trip_distance(self.rng)

        # create drivers and activate trips that start at time zero
        n_drivers = self.population_fn(self.iteration)
        self.drivers: List[Driver] = [
            Driver.build(i, init_balance(), init_start(), init_trip())
            for i in range(n_drivers)]
        self.current_time = 0
        self.sampled_delays: Dict[str, int] = {}
        self.auctions: List[List[Driver]] = []
        self.auction_lookup: Dict[str, int] = []
        self.activate_new_trips()
        obs = self.generate_observations()
        self.stats = {
            'iteration': self.iteration,
            'n_drivers': n_drivers,
            'sum_initial_balance': sum([d.balance for d in self.drivers])
        }

        msg = (
            f'starting episode {self.iteration} at time {self.current_time} '
            f'with {n_drivers} drivers'
        )
        print(msg)

        # for d in sorted(self.drivers, key=lambda d: d.trip_start_time):
        #     print(d)

        return obs

    def log_report_row(self, row):
        if self.report_logger is not None:
            self.report_logger.writerow(row)
            self.f.flush()

    def done(self):
        return all([d.done for d in self.drivers])

    def activate_new_trips(self):
        newly_activated = 0
        for driver in self.drivers:
            if driver.trip_start_time == self.current_time:
                updated = driver.start_trip(self.current_time)
                self.drivers[updated.driver_id] = updated
                newly_activated += 1
        return newly_activated

    def move_active_trips(self):
        n_active = len([d for d in self.drivers if d.active])
        moved = 0
        for driver in self.drivers:
            if driver.active:
                distance = self.scenario.sample_move_distance(
                    n_active, driver, self.rng)
                updated = driver.move(distance).update_if_arrived()
                self.drivers[updated.driver_id] = updated
                moved += 1
                # print(
                #     f'time {self.current_time} - driver {driver.driver_id} moved and '
                #     f'has traversed {updated.trip_pct()*100:.2f}% of trip'
                # )
        return moved

    def create_auctions(self) -> Tuple[int, float]:
        # create batches from active drivers
        active = [d for d in self.drivers
                  if d.active and not d.replannings >= self.max_replannings]
        n_active = len(active)
        if n_active < 2:
            return 0, 0.0

        batches = self.scenario.create_batches(active, self.rng)

        # place batches into auctions where there are at least 2 drivers
        self.auctions = []
        for batch in batches:
            if len(batch) > 1:  # could put other filter criteria here in the future
                auction_id = len(self.auctions)
                ids = {d.driver_id: auction_id for d in batch}
                self.auction_lookup.update(ids)
                self.auctions.append(batch)

        # dish out delays to each driver that made it into an auction
        self.sampled_delays = {}
        for driver in [d for batch in batches for d in batch]:
            delay = self.scenario.sample_delay_increment(
                n_active, driver, self.max_trip_increase_pct, self.rng
            )
            self.sampled_delays.update({driver.driver_id: delay})

        # report number of auctions and percent of active agents placed in auctions
        in_auction = len([d for a in self.auctions for d in a])
        auction_pct = in_auction / n_active
        n_auctions = len(self.auctions)
        return n_auctions, auction_pct

    def resolve_auctions(self, bids: Dict[str, int]):

        def delay_fn(d: Driver) -> Driver:
            delay = self.sampled_delays.get(d.driver_id)
            updated = d.reroute(delay, self.current_time)
            return updated

        # resolve each auction and store the resulting driver states
        for auction in self.auctions:
            auction_result = resolve_winner_pay_all_auction(
                auction, 
                bids, 
                self.network_signal_fn, 
                delay_fn, 
                self.max_balance)

            for driver_update in auction_result:
                self.drivers[driver_update.driver_id] = driver_update

    def clear_auctions(self):
        self.auctions = []
        self.auction_lookup = {}
        self.sampled_delays = {}

    def generate_observations(self):
        """
        generate an observation for each driver that is participating in an auction.
        since they are in an auction, they should have had a sampled_delay generated
        in the create_auctions method.
        """
        return {d.driver_id_str: self.observation_fn(d, self.sampled_delays.get(d.driver_id)) 
                for a in self.auctions for d in a}

    def generate_rewards(self, done: bool):
        if not done:
            r = {d.driver_id_str: 0.0 for a in self.auctions for d in a}
            return r, []
        else:
            allocations = [d.pct_original_of_final() for d in self.drivers]
            fairness = jain_user_fairness(allocations)
            rewards = {d.driver_id_str: f for d,
                       f in zip(self.drivers, fairness)}

            return rewards, allocations

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

    def report_state(self):
        states = [
            DriverState.INACTIVE,
            DriverState.ACTIVE,
            DriverState.DONE,
        ]
        acc = {s.name: 0 for s in states}
        for d in self.drivers:
            acc[d.state.name] = acc[d.state.name] + 1
        report = [k + ": " + str(v).ljust(5) for k, v in acc.items()]
        return ' '.join(report)

    def append_final_stats(self, rewards, allocations):

        # karma stats
        sum_balance = sum([d.balance for d in self.drivers])
        mean_balance = stats.mean([d.balance for d in self.drivers])
        mean_balance_diff = stats.mean([d.balance_diff() for d in self.drivers])
        std_balance_diff = stats.stdev([d.balance_diff() for d in self.drivers])

        # reward stats
        mean_allocation = stats.mean(allocations)
        std_allocation = stats.stdev(allocations)
        mean_alloc_replanned = stats.mean([d.pct_original_of_final()
                                            for d in self.drivers
                                            if d.replannings > 0])
        mean_reward = stats.mean(rewards.values())
        
        # trip stats
        mean_orig_trip = stats.mean(
            [d.original_trip_total for d in self.drivers])
        mean_exp_trip = stats.mean([d.trip_total() for d in self.drivers])
        mean_replannings = stats.mean([d.replannings for d in self.drivers])
        mean_delay = stats.mean([d.delay for d in self.drivers])
        mean_delay_pct = stats.mean([d.delay_pct() for d in self.drivers])
        std_delay_pct = stats.stdev([d.delay_pct() for d in self.drivers])
        mean_increase = stats.mean([d.delay_offset_pct() for d in self.drivers])
        
        self.stats['sum_final_balance'] = sum_balance
        self.stats['mean_balance'] = mean_balance
        self.stats['mean_balance_diff'] = mean_balance_diff
        self.stats['std_balance_diff'] = std_balance_diff
        
        self.stats['mean_allocation'] = mean_allocation
        self.stats['std_allocation'] = std_allocation
        self.stats['mean_allocation_replanned_only'] = mean_alloc_replanned
        self.stats['mean_reward'] = mean_reward

        self.stats['mean_original_trip_distance'] = mean_orig_trip
        self.stats['mean_experienced_trip_distance'] = mean_exp_trip
        self.stats['mean_replannings'] = mean_replannings
        self.stats['mean_delay'] = mean_delay
        self.stats['mean_delay_pct'] = mean_delay_pct
        self.stats['std_delay_pct'] = std_delay_pct
        self.stats['mean_increase'] = mean_increase
        