from __future__ import annotations

from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


class NetworkObsSpace(Enum):
    SPEED = 0
    SPEED_DIFF = 1
    SPEED_RELATIVE = 2

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return NetworkObsSpace[s.upper()]
        except KeyError:
            return s

    @classmethod
    def get_feature(cls, driver_obs_space: NetworkObsSpace) -> Optional[list]:
        """
        mapping from feature names listed in DriverPolicySpace.scala
        to their possible range as low/high values
        """
        _OBSERVATION_SPACE_MAPPING = {
            NetworkObsSpace.SPEED: [0, np.inf],
            NetworkObsSpace.SPEED_DIFF: [np.NINF, 0],
            NetworkObsSpace.SPEED_RELATIVE: [0, 1]
        }
        return _OBSERVATION_SPACE_MAPPING.get(driver_obs_space)


def build_observation_space(
        feature_names: list[NetworkObsSpace],
        agents: Optional[List[str]] = None) -> spaces.Space:

    # grab the range of values for each feature requested
    low, high = [], []
    for name in feature_names:
        feature_range = NetworkObsSpace.get_feature(name)
        if feature_range is None:
            alts = ", ".join(NetworkObsSpace)
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
