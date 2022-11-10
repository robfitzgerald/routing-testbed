from ray.rllib.policy.policy import Policy

# https://docs.ray.io/en/latest/rllib/rllib-concepts.html
# https://github.com/ray-project/ray/tree/master/rllib/examples/policy


class DriverSimpleBidder(Policy):

    def __init__(
            self,
            delay_feature_index,
            delay_threshold,
            bid_increment=10,
            risk_feature_index=None,
            risk_threshold=None,
            *args,
            **kwargs):

        has_risk_index = risk_feature_index is not None
        has_risk_thresh = risk_threshold is not None
        include_risk = has_risk_index and has_risk_thresh
        risk_inputs_match = has_risk_index != has_risk_thresh

        if not risk_inputs_match:
            msg = (
                "risk feature index and threshold must both be included "
                "or dis-included (logical XOR style)"
            )
            raise RuntimeError(msg)

        super().__init__(*args, **kwargs)
        self.delay_feature_index = delay_feature_index
        self.delay_threshold = delay_threshold
        self.include_risk = include_risk
        self.risk_feature_index = risk_feature_index
        self.risk_threshold = risk_threshold
        self.bid_increment = bid_increment

    def action_selection_heuristic(self, obs):
        """
        a heuristic for bid selection that bids if we are delayed, and additionally
        bids more if we are encountering risk. in this implementation, risk is optional.

        example: given a bid_increment of 10, risk => +10, delay => +10, so bid values
        may be {0, 10, 20}.

        :param obs: observation vector 
        :returns: the bid to make
        """
        bid = 0

        if obs[self.delay_feature_index] > self.delay_threshold:
            bid += self.bid_increment

        if self.include_risk:
            if obs[self.risk_feature_index] > self.risk_threshold:
                bid += self.bid_increment

        return bid

    def compute_actions(
            self,
            obs_batch=None,
            state_batches=None,
            prev_action_batch=None,
            prev_reward_batch=None,
            info_batch=None,
            episodes=None,
            explore=None,
            timestep=None,
            **kwargs):

        if obs_batch is None:
            return [], [], {}
        else:
            bids = [self.action_selection_heuristic(obs) for obs in obs_batch]
            return bids, [], {}
