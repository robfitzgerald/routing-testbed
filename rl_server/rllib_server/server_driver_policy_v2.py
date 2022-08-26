#!/usr/bin/env python
"""
Example of running an RLlib policy server, allowing connections from
external environment running clients. The server listens on
(a simple CartPole env
in this case) against an RLlib policy server listening on one or more
HTTP-speaking ports. See `cartpole_client.py` in this same directory for how
to start any number of clients (after this server has been started).

This script will not create any actual env to illustrate that RLlib can
run w/o needing an internalized environment.

Setup:
1) Start this server:
    $ python cartpole_server.py --num-workers --[other options]
      Use --help for help.
2) Run n policy clients:
    See `cartpole_client.py` on how to do this.

The `num-workers` setting will allow you to distribute the incoming feed over n
listen sockets (in this example, between 9900 and 990n with n=worker_idx-1).
You may connect more than one policy client to any open listen port.
"""


import os

import ray
from ray import tune
from ray.rllib.agents.registry import get_trainer_class
from rl_server.rllib_server.driver_cli import parser

from ray.rllib.examples.custom_metrics_and_callbacks import MyCallbacks
from ray.tune.logger import pretty_print

# from ray.rllib.env.policy_server_input import PolicyServerInput
from rl_server.so_routing.env.no_pickle_v3 import PolicyServerInput
from rl_server.so_routing.env.driver_policy.driver_obs_space import DriverObsSpace, build_observation_space

SERVER_ADDRESS = "localhost"
# In this example, the user can run the policy server with
# n workers, opening up listen ports 9900 - 990n (n = num_workers - 1)
# to each of which different clients may connect.
SERVER_BASE_PORT = 9900  # + worker-idx - 1

CHECKPOINT_FILE = "last_checkpoint_{}.out"


def run():
    args = parser.parse_args()
    ray.init()

    # `InputReader` generator (returns None if no input reader is needed on
    # the respective worker).
    def _input(ioctx):
        # We are remote worker or we are local worker with num_workers=0:
        # Create a PolicyServerInput.
        if ioctx.worker_index > 0 or ioctx.worker.num_workers == 0:
            return PolicyServerInput(
                ioctx,
                SERVER_ADDRESS,
                args.port + ioctx.worker_index -
                (1 if ioctx.worker_index > 0 else 0),
            )
        # No InputReader (PolicyServerInput) needed.
        else:
            return None

    try:
        feature_list = args.feature_names.split(',')
        obs_space_names = list(map(lambda s: DriverObsSpace[s], feature_list))
    except Exception as e:
        raise Exception(f"failed parsing observation features") from e

    obs_space = build_observation_space(obs_space_names, args.max_account)
    act_space = args.action_space.action_space(args.max_bid)

    print("observation space")
    print(obs_space)
    print("action space")
    print(act_space)

    # Trainer config. Note that this config is sent to the client only in case
    # the client needs to create its own policy copy for local inference.
    config = {
        # Indicate that the Trainer we setup here doesn't need an actual env.
        # Allow spaces to be determined by user (see below).
        "env": None,
        # TODO: (sven) make these settings unnecessary and get the information
        #  about the env spaces from the client.
        "observation_space": obs_space,
        "action_space": act_space,
        # Use the `PolicyServerInput` to generate experiences.
        "input": _input,
        # Number of rollout worker actors to create for parallel sampling. Setting
        # this to 0 will force rollouts to be done in the trainer actor.
        "num_workers": args.num_workers,
        # How to build per-Sampler (RolloutWorker) batches, which are then
        # usually concat'd to form the train batch. Note that "steps" below can
        # mean different things (either env- or agent-steps) and depends on the
        # `count_steps_by` (multiagent) setting below.
        # truncate_episodes: Each produced batch (when calling
        #   RolloutWorker.sample()) will contain exactly `rollout_fragment_length`
        #   steps. This mode guarantees evenly sized batches, but increases
        #   variance as the future return must now be estimated at truncation
        #   boundaries.
        # complete_episodes: Each unroll happens exactly over one episode, from
        #   beginning to end. Data collection will not stop unless the episode
        #   terminates or a configured horizon (hard or soft) is hit.
        "batch_mode": "complete_episodes",
        # Number of environments to evaluate vector-wise per worker. This enables
        # model inference batching, which can improve performance for inference
        # bottlenecked workloads.
        # "num_envs_per_worker": 0,
        # Disable OPE, since the rollouts are coming from online clients.
        "input_evaluation": [],
        # Create a "chatty" client/server or not.
        "callbacks": MyCallbacks if args.callbacks_verbose else None,
        # DL framework to use.
        "framework": args.framework,
        # Set to INFO so we'll see the server's actual address:port.
        "log_level": "INFO",
        "model": {},
    }

    # DQN.
    if args.run == "DQN" or args.run == "APEX" or args.run == "R2D2":
        # Example of using DQN (supports off-policy actions).
        config.update(
            {
                "learning_starts": 0,
                "timesteps_per_iteration": 200,
                "n_step": 3,
                "rollout_fragment_length": 200,
                "train_batch_size": 200,
            }
        )
        config["model"] = {
            "fcnet_hiddens": [64],
            "fcnet_activation": "linear",
        }
        if args.run == "R2D2":
            config["model"]["use_lstm"] = args.use_lstm

    elif args.run == "IMPALA":
        config.update(
            {
                "num_gpus": 0,
                "model": {"use_lstm": args.use_lstm},
            }
        )

    # PPO.
    else:
        # Example of using PPO (does NOT support off-policy actions).
        config.update(
            {
                "rollout_fragment_length": 1000,
                "train_batch_size": 4000,
                "model": {"use_lstm": args.use_lstm},
            }
        )

    checkpoint_path = CHECKPOINT_FILE.format(args.run)
    # Attempt to restore from checkpoint, if possible.
    if not args.no_restore and os.path.exists(checkpoint_path):
        checkpoint_path = open(checkpoint_path).read()
    else:
        checkpoint_path = None

    # Manual training loop (no Ray tune).
    if args.no_tune:
        trainer_cls = get_trainer_class(args.run)
        trainer = trainer_cls(config=config)

        if checkpoint_path:
            print("Restoring from checkpoint path", checkpoint_path)
            trainer.restore(checkpoint_path)

        # Serving and training loop.
        ts = 0
        for _ in range(args.stop_iters):
            results = trainer.train()
            print(pretty_print(results))
            checkpoint = trainer.save()
            print("Last checkpoint", checkpoint)
            with open(checkpoint_path, "w") as f:
                f.write(checkpoint)
            if (
                results["episode_reward_mean"] >= args.stop_reward
                or ts >= args.stop_timesteps
            ):
                break
            ts += results["timesteps_total"]

    # Run with Tune for auto env and trainer creation and TensorBoard.
    else:
        stop = {
            "training_iteration": args.stop_iters,
            "timesteps_total": args.stop_timesteps,
            # "episode_reward_mean": args.stop_reward,
        }

        tune.run(args.run, config=config, stop=stop,
                 verbose=2, restore=checkpoint_path)
