
import random
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from typing import Callable, List
from enum import Enum
import math


class NetworkSignalFunctionType(Enum):
    UNIFORM = 0
    FIXED_FIFTY_PERCENT = 1

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
