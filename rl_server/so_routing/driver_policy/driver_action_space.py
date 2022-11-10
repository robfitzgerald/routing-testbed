from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


def build_action_space(space_idx: int, max_bid: int, agents: Optional[List[str]] = None) -> spaces.Space:

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
        space = _SPACE[space_idx]
    except IndexError as e:
        msg = (
            f'no space with provided index {space_idx}, please pick '
            f'an index in the range [0, {len(_SPACE)})'
        )
        raise IndexError(msg) from e

    if agents is None:
        return space
    else:
        # expand into a multiagent space
        mapping = {agent: space for agent in agents}
        return spaces.Dict(mapping)


class DriverActionSpace(Enum):
    DISCRETE = 0
    CONTINUOUS = 1

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return DriverActionSpace[s.upper()]
        except KeyError:
            return s

    def action_space(self, max_bid: int, agent_list: Optional[list[str]]) -> spaces.Space:
        return build_action_space(self.value, max_bid, agent_list)


class DriverSpaceEncoder(JSONEncoder):
    def default(self, o):
        return o.name
