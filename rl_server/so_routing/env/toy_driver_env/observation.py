from typing import Callable, List
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from rl_server.so_routing.driver_policy.driver_obs_space import DriverObsSpace, MAX_UO_SO_OFFSET


def get_driver_obs(obs_type: DriverObsSpace, driver: Driver, sampled_delay: int) -> float:
    """
    observes a driver based on the provided DriverObsSpace observation type. includes
    the driver
    """
    if obs_type == DriverObsSpace.BALANCE:
        return float(driver.balance)
    elif obs_type == DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME:
        return float(driver.delay)
    elif obs_type == DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME_PCT:
        return max(0.0, min(1.0, driver.delay_offset_pct()))
    elif obs_type == DriverObsSpace.MARGINAL_OFFSET_FREE_FLOW_UO_TO_SO:
        # (so-ff) - (uo-ff)) / (uo-ff) simplified to (-u + s) / (u - f)
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
    else:
        raise TypeError(f'unsupported driver space feature {obs_type}')


def compose_observation_fn(features: List[DriverObsSpace]) -> Callable[[Driver, int], list[float]]:
    def _fn(d: Driver, sampled_delay: int) -> list[float]:
        return [get_driver_obs(o, d, sampled_delay) for o in features]
    return _fn
