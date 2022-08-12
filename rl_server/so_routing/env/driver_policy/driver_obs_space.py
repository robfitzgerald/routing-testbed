from __future__ import annotations

from enum import Enum
from typing import Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


def build_observation_space(feature_names: list[DriverObsSpace], max_karma: int) -> spaces.Box:
    # mapping from feature names listed in DriverPolicySpace.scala
    # to their low/high value ranges
    _OBSERVATION_SPACE_MAPPING = {
        "balance": [0, max_karma],
        "urgency": [np.NINF, np.Inf],
        "worst_alternative": [np.NINF, np.Inf],
        "batch_size": [0, np.Inf]
    }

    # grab the range of values for each feature requested
    low, high = [], []
    for name in feature_names:
        feature_range = _OBSERVATION_SPACE_MAPPING.get(str(name))
        if feature_range is None:
            alts = ", ".join(DriverObsSpace)
            msg = f"unknown feature name {str(name)}, must be one of {{{alts}}}"
            raise KeyError(msg)
        l, h = feature_range
        low.append(l)
        high.append(h)

    # construct the gym.spaces.Box for this run
    n_dims = len(feature_names)
    space = spaces.Box(
        low=np.array(low),
        high=np.array(high),
        shape=(n_dims,),
        dtype=np.float64
    )

    return space


class DriverObsSpace(Enum):
    BALANCE = 0
    URGENCY = 1
    WORST_ALTERNATIVE = 2
    BATCH_SIZE = 3

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return DriverObsSpace[s.upper()]
        except KeyError:
            return s


class DriverObsSpaceEncoder(JSONEncoder):
    def default(self, o):
        return o.name
