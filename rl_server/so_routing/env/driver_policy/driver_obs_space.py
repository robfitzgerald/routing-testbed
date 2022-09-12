from __future__ import annotations

from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


class DriverObsSpace(Enum):
    BALANCE = 0
    URGENCY = 1
    WORST_ALTERNATIVE = 2
    BATCH_SIZE = 3
    THRESHOLD_NETWORK_SIGNAL = 4
    # maybe other network signals?
    REMAINING_DISTANCE = 6
    REMAINING_TRAVEL_TIME_ESTIMATE = 7
    REPLANNING_EVENTS = 8

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

    @classmethod
    def get_feature(cls, driver_obs_space: DriverObsSpace, max_karma: int) -> Optional[list]:
        """
        mapping from feature names listed in DriverPolicySpace.scala
        to their possible range as low/high values
        """
        _OBSERVATION_SPACE_MAPPING = {
            DriverObsSpace.BALANCE: [0, max_karma],
            DriverObsSpace.URGENCY: [np.NINF, np.Inf],
            DriverObsSpace.WORST_ALTERNATIVE: [np.NINF, np.Inf],
            DriverObsSpace.BATCH_SIZE: [0, np.Inf],
            DriverObsSpace.THRESHOLD_NETWORK_SIGNAL: [0, 1],
            DriverObsSpace.REMAINING_DISTANCE: [0, np.Inf],
            DriverObsSpace.REMAINING_TRAVEL_TIME_ESTIMATE: [0, np.Inf],
            DriverObsSpace.REPLANNING_EVENTS: [0, np.Inf]
        }
        return _OBSERVATION_SPACE_MAPPING.get(driver_obs_space)


def build_observation_space(
        feature_names: list[DriverObsSpace],
        max_karma: int,
        agents: Optional[List[str]] = None) -> spaces.Space:

    # grab the range of values for each feature requested
    low, high = [], []
    for name in feature_names:
        feature_range = DriverObsSpace.get_feature(name, max_karma)
        if feature_range is None:
            alts = ", ".join(DriverObsSpace)
            msg = f"unknown feature name {str(name)}, must be one of {{{alts}}}"
            raise KeyError(msg)
        l, h = feature_range
        if isinstance(l, list):
            low.extend(l)
        else:
            low.append(l)
        if isinstance(h, list):
            high.extend(h)
        else:
            high.append(h)

    # construct the gym.spaces.Box for this run
    n_dims = len(feature_names)
    space = spaces.Box(
        low=np.array(low),
        high=np.array(high),
        shape=(n_dims,),
        dtype=np.float64
    )

    if agents is None:
        return space
    else:
        # expand into a multiagent space
        mapping = {agent: space for agent in agents}
        return spaces.Dict(mapping)


class DriverObsSpaceEncoder(JSONEncoder):
    def default(self, o):
        return o.name
