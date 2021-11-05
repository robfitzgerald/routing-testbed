import logging
from typing import Dict, Callable, NamedTuple

from ray.rllib.env.policy_client import PolicyClient
from gym import Space

from relay_server.proto.proto_helper import ProtoHelper
import relay_server.proto.external_assignment_v1_pb2 as proto

log = logging.getLogger(__name__)


class PreviousState(NamedTuple):
    action: int
    reward: float


class V1Ops(ProtoHelper):

    def __init__(self,
                 url,
                 port,
                 obs_fn: Callable[[proto.AgentObservation], Space],
                 k: int,
                 training_enabled=True,
                 inference_mode="remote"):
        super(V1Ops, self).__init__(
            action_request_constructor=proto.ActionRequest,
            action_response_constructor=proto.ActionResponse,
            reward_request_constructor=proto.RewardRequest,
            reward_response_constructor=proto.RewardResponse
        )
        uri = f"{url}:{port}"
        log.info(f"connecting to rl server at {uri}")
        self.client = PolicyClient(f"{url}:{port}", inference_mode=inference_mode)
        self.session_id = self.client.start_episode(training_enabled=training_enabled)
        self.obs_fn = obs_fn
        self.k = k
        # we store a mapping from each agent to the current
        # state, as it corresponds to these phases:
        # 1. receiving new ActionRequest - should hold the previous state
        # 2. sending ActionResponse - we selected an action for each agent based on the previous state, if any,
        #      along with the next state; after selecting an action, we replace the state
        # 3. receiving RewardRequest - we use the stored obs/act along with the provided reward
        # 4. sending RewardResponse - clear the state
        self.state: dict[str, PreviousState] = {}

    def perform_operation(self, req):
        """
        perform the action - generating actions, or, updating the model with rewards
        :param req: the parsed, protobuf-generated request class
        :return: the response object - an ActionResponse or RewardResponse
        :raises: Exception
        """
        if isinstance(req, proto.ActionRequest):

            # collect data from next + prev observations
            # prev_actions = {agent_id: prev_state.action for agent_id, prev_state in self.state}
            # prev_rewards = {agent_id: prev_state.reward for agent_id, prev_state in self.state}
            new_obs = {row.agent: self.obs_fn(row) for row in req.observations}

            def to_act_selection(agent_id, action) -> proto.ActionSelection:
                a = proto.ActionSelection()
                a.agent = agent_id
                a.action = action
                return a

            actions = self.client.get_action(self.session_id, new_obs)

            res = proto.ActionResponse()
            res.session_id = req.session_id
            res.actions = [to_act_selection(agent, action) for agent, action in actions.items()]

            return res

        elif isinstance(req, proto.RewardRequest):
            # update the model with the reward for the provided action
            # self.trainer
            rewards = {row.agent: row.reward for row in req.rewards}
            self.client.log_returns(self.session_id, rewards)

            res = proto.RewardResponse()
            res.session_id = self.session_id

            return res

        else:
            raise Exception(f"unexpected request type {type(req)}")
