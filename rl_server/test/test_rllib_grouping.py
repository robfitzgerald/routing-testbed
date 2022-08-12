import unittest

from gym import spaces
from ray.rllib.env import ExternalMultiAgentEnv
from ray.rllib.env.wrappers.group_agents_wrapper import GroupAgentsWrapper

from rllib.env.space_v1 import create_space


class TestGrouping(unittest.TestCase):

    class MockEnv(ExternalMultiAgentEnv):

        def __init__(self, grouping, k):

            obs_space, act_space = create_space(grouping, k)
            self.observation_space = obs_space
            self.action_space = act_space
            super().__init__(act_space, obs_space)

        def run(self):
            pass

    def test_whats_in_grouped(self):
        grouping = {
            "group_1": [ "agent_1", "agent_2"],
            "group_2": [ "agent_3", "agent_4"]
        }
        env = self.MockEnv(grouping, k=10)
        wrapped = GroupAgentsWrapper(env, grouping, env.observation_space, env.action_space)
        wrapped

        obs = {
            "agent_1": [0.0, 0.0, 10.0, 10.0, 0.10],
            "agent_2": [10.0, 10.0, 0.0, 00.0, 0.02]
        }

        result = wrapped._group_items(obs)
        print(result)

        result2 = wrapped._ungroup_items(result)
        print(result2)