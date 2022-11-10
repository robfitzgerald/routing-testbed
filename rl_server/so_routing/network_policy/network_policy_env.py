import json
from pathlib import Path
from typing import Dict

import gym
from gym.spaces import Tuple
from ray.rllib import MultiAgentEnv
from ray.rllib.env import GroupAgentsWrapper
from ray.rllib.env.external_multi_agent_env import ExternalMultiAgentEnv
from rl_server.so_routing import space_v1

# deprecated


def load_grouping(grouping_path):
    if not grouping_path.is_file():
        raise IOError(f'groupings file {grouping_path} does not exist')

    with grouping_path.open('r') as f:
        grouping = json.loads(f.read())

    return grouping


def create_spaces(k: int):

    obs_space, act_space = space_v1.create_space(k)

    return obs_space, act_space


def create_so_routing_multiagent_config(grouping, obs_space, act_space):
    policies = {}
    for group_id, agent_ids in grouping.items():
        o = Tuple([obs_space for agent_id in agent_ids])
        a = Tuple([act_space for agent_id in agent_ids])
        policies.update({group_id: (None, o, a, {})})

    return {
        "policies": policies,
        "policy_mapping_fn": lambda group_id: group_id
    }


def create_so_routing_env(clients: int,
                          k: int,
                          grouping_path: Path):
    if not grouping_path.is_file():
        raise IOError(f'groupings file {grouping_path} does not exist')

    with grouping_path.open('r') as f:
        grouping = json.loads(f.read())

    obs_space, act_space = space_v1.create_space(grouping, k)

    # construct as an ExternalMultiAgentEnv
    env = ExternalMultiAgentEnv(act_space, obs_space, clients)
    env = GroupAgentsWrapper(env, groups=grouping,
                             obs_space=obs_space, act_space=act_space)

    # env.groups  # defined, comes from GroupAgentsWrapper

    # env.run  # defined, comes from ExternalMultiAgentEnv

    return env


class SoRoutingEnv(ExternalMultiAgentEnv, MultiAgentEnv):

    def __init__(self,
                 act_space: gym.Space,
                 obs_space: gym.Space,
                 max_concurrent: int):

        super().__init__(act_space, obs_space, max_concurrent)

    def run(self):
        pass
