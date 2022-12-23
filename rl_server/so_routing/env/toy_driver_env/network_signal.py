
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
    def _fn(drivers: List[Driver]) -> float:
        return random.random()
    return _fn


def create_fixed_50_signal_fn():
    def _fn(drivers: List[Driver]) -> float:
        return 0.5
    return _fn


def create_thresh_signal_fn(thresh):
    assert thresh > 0, f"thresh must be positive, found {thresh}"

    def _fn(drivers: List[Driver]) -> float:
        pct_sig = min(1.0, float(len(drivers)) / thresh)
        return pct_sig
    return _fn
