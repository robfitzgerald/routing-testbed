from __future__ import annotations

from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


# when comparing the SO and UO trips, we ignore increase beyond this
# factor, which would be extreme outliers.
MAX_UO_SO_OFFSET = 4


class DriverObsSpace(Enum):
    BALANCE = 0
    BATCH_SIZE = 1
    REPLANNING_EVENTS = 2
    AUCTION_WIN_RATE = 3

    ORIGINAL_DISTANCE = 10
    EXPERIENCED_DISTANCE = 11
    REMAINING_DISTANCE = 12
    PERCENT_DISTANCE = 13
    MARGINAL_UO_DISTANCE = 14
    MARGINAL_WORST_SO_DISTANCE = 15

    ORIGINAL_TRAVEL_TIME_ESTIMATE = 20
    EXPERIENCED_TRAVEL_TIME = 21
    REMAINING_TRAVEL_TIME_ESTIMATE = 22
    MARGINAL_UO_TRAVEL_TIME = 23
    MARGINAL_WORST_SO_TRAVEL_TIME = 24
    FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME = 25
    FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME_PCT = 26
    FREE_FLOW_DIFF_UO_TRAVEL_TIME = 27
    FREE_FLOW_DIFF_WORST_SO_TRAVEL_TIME = 28
    ORIGINAL_TRAVEL_TIME_DIFF = 29
    WORST_ALTERNATIVE = 30
    MARGINAL_OFFSET_FREE_FLOW_UO_TO_SO = 31
    MARGINAL_OFFSET_UO_TO_SO = 32

    # maybe other network signals?
    THRESHOLD_NETWORK_SIGNAL = 40

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
            DriverObsSpace.BATCH_SIZE: [0, np.Inf],
            DriverObsSpace.REPLANNING_EVENTS: [0, np.Inf],
            DriverObsSpace.AUCTION_WIN_RATE: [0, 1],

            DriverObsSpace.ORIGINAL_DISTANCE: [0, np.Inf],
            DriverObsSpace.EXPERIENCED_DISTANCE: [0, np.Inf],
            DriverObsSpace.REMAINING_DISTANCE: [0, np.Inf],
            DriverObsSpace.PERCENT_DISTANCE: [0, 1],
            DriverObsSpace.MARGINAL_UO_DISTANCE: [np.NINF, np.Inf],
            DriverObsSpace.MARGINAL_WORST_SO_DISTANCE: [np.NINF, np.Inf],

            DriverObsSpace.ORIGINAL_TRAVEL_TIME_ESTIMATE: [0, np.Inf],
            DriverObsSpace.EXPERIENCED_TRAVEL_TIME: [0, np.Inf],
            DriverObsSpace.REMAINING_TRAVEL_TIME_ESTIMATE: [0, np.Inf],
            DriverObsSpace.MARGINAL_UO_TRAVEL_TIME: [np.NINF, np.Inf],
            DriverObsSpace.MARGINAL_WORST_SO_TRAVEL_TIME: [np.NINF, np.Inf],
            DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME: [np.NINF, np.Inf],
            DriverObsSpace.FREE_FLOW_DIFF_EXPERIENCED_TRAVEL_TIME_PCT: [0, 1],
            DriverObsSpace.FREE_FLOW_DIFF_UO_TRAVEL_TIME: [np.NINF, np.Inf],
            DriverObsSpace.FREE_FLOW_DIFF_WORST_SO_TRAVEL_TIME: [np.NINF, np.Inf],
            DriverObsSpace.ORIGINAL_TRAVEL_TIME_DIFF: [np.NINF, np.Inf],
            DriverObsSpace.WORST_ALTERNATIVE: [np.NINF, np.Inf],
            DriverObsSpace.MARGINAL_OFFSET_FREE_FLOW_UO_TO_SO: [0, MAX_UO_SO_OFFSET],
            DriverObsSpace.MARGINAL_OFFSET_UO_TO_SO: [0, MAX_UO_SO_OFFSET],

            DriverObsSpace.THRESHOLD_NETWORK_SIGNAL: [0, 1],
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
