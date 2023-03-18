from enum import Enum
from typing import List, Optional
from gym import spaces
import numpy as np
from json import JSONEncoder
import logging

log = logging.getLogger(__name__)


def build_space_instance(space_idx: int, max_k: Optional[int] = None, dims: int = 1):
    _SPACE = [
        # DISCRETE SPACES
        spaces.Discrete(max_k if max_k is not None else 1),
        # CONTINUOUS SPACES
        spaces.Box(
            low=np.array([0.0 for _ in range(dims)]),
            high=np.array([1.0 for _ in range(dims)]),
            shape=(dims,),
            dtype=np.float32
        )
    ]
    try:
        space = _SPACE[space_idx]
        return space
    except IndexError as e:
        msg = (
            f'no space with provided index {space_idx}, please pick '
            f'an index in the range [0, {len(_SPACE)})'
        )
        raise IndexError(msg) from e


def build_action_space(
        space_idx: int,
        max_k: Optional[int] = None,
        agents: Optional[List[str]] = None,
        n_agents: Optional[int] = None) -> spaces.Space:

    if n_agents is not None and agents is not None:
        msg = (
            f'build_observation_space called with both "agents" and '
            f'"n_agents" specified, cannot submit both. agents is used '
            f'for multiagent spaces and n_agents for tuple spaces.'
        )
        raise KeyError(msg)
    if n_agents is None and agents is None:
        msg = (
            f'must specify either "agents" or "n_agents". agents is used '
            f'for multiagent spaces and n_agents for tuple spaces.'
        )
        raise KeyError(msg)
    multiagent = agents is not None

    if space_idx == 0 and max_k is None:
        raise ValueError("discrete action space requires a k value")

    if multiagent:
        log.info(f'building multiagent action space of type spaces.Dict')
        space = build_space_instance(space_idx, max_k, dims=1)
        mapping = {agent: space for agent in agents}
        dict_space = spaces.Dict(mapping)
        log.info(dict_space)
        return dict_space
    else:
        log.info(
            f'building tupled action space of type spaces.Tuple, vector size {n_agents}:')
        space = build_space_instance(space_idx, dims=n_agents)
        # vector = [space for _ in range(n_agents)]
        log.info(space)
        # return spaces.Tuple(vector)
        return space


def build_marl_act_space(n_agents: int) -> spaces.Space:
    """
    the implementation for QMIX in RLlib requires the action space is a spaces.Tuple.
    this makes sense; we flatten the separated agents into a Tuple of observations without
    the agent id. 
    see Grouping.
    """
    obs = build_action_space(1)  # assumes NetworkActionSpace.CONTINUOUS
    return spaces.Tuple([obs for _ in range(n_agents)])


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
