from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder


def build_action_space(
        space_idx: int,
        max_k: Optional[int] = None,
        agents: Optional[List[str]] = None) -> spaces.Space:

    if space_idx == 0 and max_k is None:
        raise ValueError("discrete action space requires a k value")

    _SPACE = [
        # DISCRETE SPACES
        spaces.Discrete(max_k if max_k is not None else 1),
        # CONTINUOUS SPACES
        spaces.Box(
            low=np.array([0.0]),
            high=np.array([1.0]),
            shape=(1,),
            dtype=np.float32
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


class NetworkActionSpace(Enum):
    DISCRETE = 0
    CONTINUOUS = 1

    def __str__(self):
        return self.name.lower()

    def __repr__(self):
        return str(self)

    @staticmethod
    def argparse(s):
        try:
            return NetworkActionSpace[s.upper()]
        except KeyError:
            return s

    def action_space(self, agent_list: Optional[list[str]], max_k: Optional[int] = None) -> spaces.Space:
        return build_action_space(self.value, max_k, agent_list)


class NetworkSpaceEncoder(JSONEncoder):
    def default(self, o):
        return o.name
