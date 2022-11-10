from typing import Callable, List
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from rl_server.so_routing.driver_policy.driver_obs_space import DriverObsSpace


def get_driver_obs(driver: Driver, obs_type: DriverObsSpace) -> float:
    if obs_type == DriverObsSpace.BALANCE:
        return float(driver.balance)
    elif obs_type == DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME:
        return float(driver.delay)
    elif obs_type == DriverObsSpace.MARGINAL_OFFSET_FREE_FLOW_UO_TO_SO:
        # (so-ff) - (uo-ff)) / (uo-ff)
        # here, ff=0
        uo = float(driver.original_trip_total)
        so = float(driver.trip_total())
        return (so - uo) / so
    elif obs_type == DriverObsSpace.EXPERIENCED_DISTANCE:
        return float(driver.trip_position)
    elif obs_type == DriverObsSpace.PERCENT_DISTANCE:
        return float(driver.trip_position) / float(driver.trip_total())
    else:
        raise TypeError(f'unsupported driver space feature {obs_type}')


def compose_observation_fn(features: List[DriverObsSpace]) -> Callable[[Driver], list[float]]:
    def _fn(d: Driver) -> list[float]:
        return [get_driver_obs(d, o) for o in features]
    return _fn
