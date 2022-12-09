
import random
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from typing import Callable, List
from enum import Enum
import math


class NetworkSignalFunctionType(Enum):
    UNIFORM = 0
    FIXED_FIFTY_PERCENT = 1
    THRESH_FIVE_DRIVERS = 2
    THRESH_TEN_DRIVERS = 3

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return NetworkSignalFunctionType[s.upper()]
        except KeyError:
            return s

    def create_fn(self) -> Callable[[List[Driver]], int]:
        if self == self.UNIFORM:
            return create_random_network_signal_fn()
        elif self == self.FIXED_FIFTY_PERCENT:
            return create_fixed_50_signal_fn()
        elif self == self.THRESH_FIVE_DRIVERS:
            return create_thresh_signal_fn(5)
        elif self == self.THRESH_TEN_DRIVERS:
            return create_thresh_signal_fn(10)
        else:
            raise Exception("internal error")


def create_random_network_signal_fn():
    def _fn(drivers: List[Driver]) -> int:
        n_drivers = len(drivers)
        sig = random.randint(0, n_drivers)
        return sig
    return _fn


def create_fixed_50_signal_fn():
    def _fn(drivers: List[Driver]) -> int:
        n_drivers = len(drivers)
        sig = int(math.floor(float(n_drivers) / 2.0))
        return sig
    return _fn


def create_thresh_signal_fn(thresh):
    assert thresh > 0, f"thresh must be positive, found {thresh}"

    def _fn(drivers: List[Driver]) -> int:
        n_drivers = len(drivers)
        pct_sig = float(n_drivers) / thresh
        losers = int(math.floor(n_drivers * pct_sig))
        winners = n_drivers - losers
        return max(0, min(len(drivers), winners))
    return _fn
