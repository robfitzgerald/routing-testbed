import math
from typing import Callable, List, Tuple
from rl_server.so_routing.env.toy_driver_env.reward import jain_index
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from rl_server.so_routing.driver_policy.driver_obs_space import DriverObsSpace, MAX_UO_SO_OFFSET


def get_driver_obs(
        obs_type: DriverObsSpace,
        driver: Driver,
        batch: List[Driver],
        network_signal: float,
        sampled_delay: int,
        batch_delays: List[int],
        max_karma: int) -> float:
    """
    observes a driver based on the provided DriverObsSpace observation type. includes
    the driver
    """
    if obs_type == DriverObsSpace.BALANCE:
        return float(driver.balance)
    if obs_type == DriverObsSpace.KARMA_HEADROOM:
        return max_karma - float(driver.balance)
    if obs_type == DriverObsSpace.BALANCE_PCT:
        return float(driver.balance) / float(max_karma)
    if obs_type == DriverObsSpace.KARMA_HEADROOM_PCT:
        return 1 - (float(driver.balance) / float(max_karma))
    elif obs_type == DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME:
        return float(driver.overall_delay())
    elif obs_type == DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME_PCT:
        return max(0.0, min(1.0, driver.overall_delay_pct()))
    elif obs_type == DriverObsSpace.MARGINAL_OFFSET_FREE_FLOW_UO_TO_SO:
        # (so-ff) - (uo-ff)) / (uo-ff) simplified to (-u + s) / (u - f)
        # 0 is best, +inf is worst (capped at MAX_UO_SO_OFFSET)
        ff = float(driver.original_trip_total)
        uo = float(driver.trip_total())
        so = float(sampled_delay + driver.trip_total())
        if uo - ff == 0.0:
            return MAX_UO_SO_OFFSET
        else:
            offset = (-uo + so) / (uo - ff)
            return max(0.0, min(MAX_UO_SO_OFFSET, offset))
    elif obs_type == DriverObsSpace.MARGINAL_OFFSET_UO_TO_SO:
        uo = float(driver.trip_total())
        so = float(sampled_delay + driver.trip_total())
        if uo == 0.0:
            return MAX_UO_SO_OFFSET
        else:
            offset = (so - uo) / uo
            return max(0.0, min(MAX_UO_SO_OFFSET, offset))
    elif obs_type == DriverObsSpace.EXPERIENCED_DISTANCE:
        return float(driver.trip_position)
    elif obs_type == DriverObsSpace.PERCENT_DISTANCE:
        return float(driver.trip_position) / float(driver.trip_total())
    elif obs_type == DriverObsSpace.PERCENT_REMAINING_DISTANCE:
        return 1 - float(driver.trip_position) / float(driver.trip_total())
    elif obs_type == DriverObsSpace.BATCH_UNFAIRNESS_EXTERNALITIES:
        # 0 is best, 1 is worst
        n_losers = int(math.floor(network_signal * len(batch)))
        n_winners = len(batch) - n_losers
        fairness_diff, _, _ = estimate_batch_fairness_externalities(
            batch, batch_delays, n_winners)
        return 1 - fairness_diff
    elif obs_type == DriverObsSpace.THRESHOLD_NETWORK_SIGNAL:
        return network_signal
    else:
        raise TypeError(f'unsupported driver space feature {obs_type}')


def compose_observation_fn(
    features: List[DriverObsSpace],
    max_karma: int
) -> Callable[[Driver, List[Driver], float, int], list[float]]:
    def _fn(d: Driver, b: List[Driver], network_signal: float, sampled_delay: int, batch_delays: List[int]) -> list[float]:
        return [get_driver_obs(o, d, b, network_signal, sampled_delay, batch_delays, max_karma) for o in features]
    return _fn


def estimate_batch_fairness_externalities(batch: List[Driver], delays: List[int], n) -> Tuple[int, int, int]:
    """
    compute the jain index for a set of delays but estimating an assignment 
    of n users for UO routes. if "fair", assign UO to agents with greatest
    delays, otherwise, do opposite.

    if it's fair, we assign a delay to the n winners (highest allocations, or, reverse=True
    if it's unfair, reverse=False

    :param batch: batch of drivers in this auction
    :param delays: potential added delay they are bidding against
    :param n: number of winners
    :returns: number in [0, 1] that represents the risk to fairness in this batch,
              where 0 is least fair and 1 is most fair
    """
    # if n == 0 or n == len(batch):
    #     return 1.0, 1.0, 0.0
    # else:
    # two possible outcomes for each agent. first is their allocation
    # as it stands now, which is 1 minus the percent of the current
    # trip which is a rerouting or congestion delay. higher is better;
    # a trip with no delay will have an allocation of 100% while a trip
    # with lots of delay will get closer to 0%. second is the same
    # measure after adding the current proposed delay. we can pass in a
    # dummy value for the current time as the modified driver is
    # immediately discarded.
    agent_ub = [1.0 - d.overall_delay_pct()
                for d in batch]
    agent_lb = [1.0 - d.reroute(delay, 0).overall_delay_pct()
                for d, delay in zip(batch, delays)]

    # assign best + worst outcomes to estimate the
    # upper and lower bound allocations for each agent.
    ub = []
    lb = []
    bw_data = sorted(zip(agent_ub, agent_lb), key=lambda costs: costs[1])
    for idx, (best, worst) in enumerate(bw_data):
        ub.append(best if idx < n else worst)
        lb.append(best if idx >= n else worst)

    # the difference in jain index values for the upper and lower
    # bounded estimates is the observation we return, where 0 is
    # best and 1 is worst. we enforce that here by truncating to
    # values in [0, 1]; sometimes in rare circumstances, this can
    # evaluate to a negative result, but these are edge cases.
    jain_ub, jain_lb = jain_index(ub), jain_index(lb)
    unfairness_diff = jain_ub - jain_lb
    unfairness = min(1.0, max(0.0, unfairness_diff))
    fairness = 1.0 - unfairness
    return fairness, jain_lb, jain_ub
