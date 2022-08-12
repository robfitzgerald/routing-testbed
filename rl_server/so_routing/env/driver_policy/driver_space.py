from enum import Enum
from typing import Optional
from gym import spaces
import numpy as np
from json import JSONEncoder

# DriverSpace ENUM, below


def build_observation_space(space_idx: int, max_karma: int) -> spaces.Box:
    _SPACE = [
        # BALANCE_AND_URGENCY
        spaces.Box(
            low=np.array([0.0, np.NINF]),
            high=np.array([max_karma, np.Inf]),
            shape=(2,),
            dtype=np.float64
        ),
        # BALANCE_URGENCY_AND_WORST_ALTERNATIVES
        spaces.Box(
            low=np.array([0.0, np.NINF, np.NINF]),
            high=np.array([max_karma, np.Inf, np.Inf]),
            shape=(3,),
            dtype=np.float64
        )
    ]
    try:
        idx_mapping = [0, 1, 0, 1]
        idx = idx_mapping[space_idx]
        return _SPACE[idx]
    except IndexError as e:
        msg = (
            f'no space with provided index {space_idx}, please pick '
            f'an index in the range [0, {len(_SPACE)})'
        )
        raise IndexError(msg) from e


def build_action_space(space_idx: int, max_bid: int) -> spaces.Space:
    _SPACE = [
        # DISCRETE SPACES
        spaces.Discrete(max_bid),
        # CONTINUOUS SPACES
        spaces.Box(
            low=np.array([0.0]),
            high=np.array([1.0]),
            shape=(1,),
            dtype=np.float64
        )
    ]
    try:
        idx_mapping = [0, 0, 1, 1]
        idx = idx_mapping[space_idx]
        return _SPACE[idx]
    except IndexError as e:
        msg = (
            f'no space with provided index {space_idx}, please pick '
            f'an index in the range [0, {len(_SPACE)})'
        )
        raise IndexError(msg) from e


class DriverPolicySpace(Enum):
    BAL_URG_DISC = 0
    BAL_URG_ALT_DISC = 1
    BAL_URG_CONT = 2
    BAL_URG_ALT_CONT = 3

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return DriverPolicySpace[s.upper()]
        except KeyError:
            return s

    def observation_space(self, max_karma: int) -> spaces.Box:
        return build_observation_space(self.value, max_karma)

    def action_space(self, max_bid: int) -> spaces.Discrete:
        return build_action_space(self.value, max_bid)


class DriverSpaceEncoder(JSONEncoder):
    def default(self, o):
        return o.name
