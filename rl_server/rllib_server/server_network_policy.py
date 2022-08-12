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

import argparse
from pathlib import Path
from typing import Optional

import gym
import os
import json

from so_routing.env import space_v1
import ray
from ray.rllib.agents.dqn import DQNTrainer
from ray.rllib.agents.ppo import PPOTrainer
from ray.rllib.agents.qmix import QMixTrainer
from ray.rllib.env.policy_server_input import PolicyServerInput
from ray.rllib.examples.custom_metrics_and_callbacks import MyCallbacks
from ray.tune.logger import pretty_print

from so_routing.env.policy_server_no_pickle import PolicyServerNoPickleInput
from so_routing.env.network_policy.network_policy_env import create_so_routing_env, create_so_routing_multiagent_config, create_spaces, \
    load_grouping

SERVER_ADDRESS = "localhost"
# In this example, the user can run the policy server with
# n workers, opening up listen ports 9900 - 990n (n = num_workers - 1)
# to each of which different clients may connect.
SERVER_BASE_PORT = 9900  # + worker-idx - 1

CHECKPOINT_FILE = "last_checkpoint_{}.out"

parser = argparse.ArgumentParser()
parser.add_argument("--run", type=str,
                    choices=["DQN", "PPO", "QMIX"], default="QMIX")
parser.add_argument(
    "--k",
    type=int,
    choices=range(1, 1_000_000),
    help="in this scenario, the number of alternative paths per agent in the ksp algorithm"
         "where the index of an alternative path [0, k) is the discrete action to be chosen.")
parser.add_argument(
    "--grouping-file",
    type=str,
    help="a JSON file which constructs the grouping in the form of Dict[GroupingId, List[AgentId]]"
)
parser.add_argument(
    "--framework",
    choices=["tf", "torch"],
    default="torch",
    help="The DL framework specifier.")
parser.add_argument(
    "--no-restore",
    action="store_true",
    help="Do not restore from a previously saved checkpoint (location of "
         "which is saved in `last_checkpoint_[algo-name].out`).")
parser.add_argument(
    "--num-workers",
    type=int,
    default=2,
    help="The number of workers to use. Each worker will create "
         "its own listening socket for incoming experiences.")
parser.add_argument(
    "--chatty-callbacks",
    action="store_true",
    help="Activates info-messages for different events on "
         "server/client (episode steps, postprocessing, etc..).")
parser.add_argument(
    "--checkpoint-path",
    help="location of checkpoint directory",
    default=None
)

if __name__ == "__main__":
    args = parser.parse_args()
    ray.init(num_cpus=1)
    print(f'ray version {ray.__version__}')

    grouping_path = Path(args.grouping_file)
    # if not grouping_path.is_file():
    #     raise IOError(f'groupings file {grouping_path} does not exist')
    #
    # with grouping_path.open('r') as f:
    #     grouping = json.loads(f.read())

    # env = create_so_routing_env(args.num_workers, args.k, grouping_path)

    grouping = load_grouping(grouping_path)
    obs_space, act_space = create_spaces(args.k)
    multiagent_conf = create_so_routing_multiagent_config(
        grouping, obs_space, act_space)

    # `InputReader` generator (returns None if no input reader is needed on
    # the respective worker).
    def _input(ioctx):
        # We are remote worker or we are local worker with num_workers=0:
        # Create a PolicyServerInput.
        if ioctx.worker_index > 0 or ioctx.worker.num_workers == 0:
            return PolicyServerNoPickleInput(
                ioctx,
                SERVER_ADDRESS,
                SERVER_BASE_PORT + ioctx.worker_index -
                (1 if ioctx.worker_index > 0 else 0),
                obs_space,
                act_space
            )
        # No InputReader (PolicyServerInput) needed.
        else:
            return None

    # Trainer config. Note that this config is sent to the client only in case
    # the client needs to create its own policy copy for local inference.
    config = {
        # Indicate that the Trainer we setup here doesn't need an actual env.
        # Allow spaces to be determined by user (see below).
        "env": None,
        # "env": env,  # trying to pass along grouping info...

        # TODO: (sven) make these settings unnecessary and get the information
        #  about the env spaces from the client.
        # "observation_space": obs_space,
        # "action_space": act_space,
        "multiagent": dict(multiagent_conf, **{
            "count_steps_by": "env_steps"
        }),
        #
        # Use the `PolicyServerInput` to generate experiences.
        "input": _input,
        # Use n worker processes to listen on different ports.
        "num_workers": args.num_workers,
        # Disable OPE, since the rollouts are coming from online clients.
        "input_evaluation": [],
        # Create a "chatty" client/server or not.
        # "callbacks": MyCallbacks if args.chatty_callbacks else None,
    }

    # DQN.
    if args.run == "DQN":
        # Example of using DQN (supports off-policy actions).
        trainer = DQNTrainer(
            config=dict(
                config, **{
                    "learning_starts": 100,
                    "timesteps_per_iteration": 200,
                    "model": {
                        "fcnet_hiddens": [64],
                        "fcnet_activation": "linear",
                    },
                    "n_step": 3,
                    "framework": args.framework,
                }))
    # QMIX.
    elif args.run == "QMIX":
        trainer = QMixTrainer(
            config=dict(config, **{
                # "num_envs_per_worker": 5,  # test with vectorization on
                "mixer": "qmix",
                "framework": args.framework,  # only "torch" allowed here
                "buffer_size": 10,  # replay buffer size, in samples.
                # see https://ai.stackexchange.com/questions/11640/how-large-should-the-replay-buffer-be
                # wait, or, in batches? from docs: "Size of the replay buffer in batches (not timesteps!)."
                # don't wait until end of episode to build a batch
                "batch_mode": "truncate_episodes",
                "rollout_fragment_length": 4,  # number of steps to be performed per rollout
                                               # https://robotics.stackexchange.com/questions/16596/what-is-the-definition-of-rollout-in-neural-network-or-openai-gym
                "train_batch_size": 32,  # number of samples sent to Trainer.train_on_batch
                # "Number of env steps to optimize for before returning" (?)
                "timesteps_per_iteration": 10,
                "learning_starts": 0
            })
        )

    # PPO.
    else:
        # Example of using PPO (does NOT support off-policy actions).
        trainer = PPOTrainer(
            config=dict(
                config, **{
                    "rollout_fragment_length": 1000,
                    "train_batch_size": 4000,
                    "framework": args.framework,
                })
        )

    # Attempt to restore from checkpoint, if possible.
    if not args.no_restore and args.checkpoint_path is not None:
        if os.path.exists(args.checkpoint_path):
            # checkpoint_path = open(checkpoint_path).read()
            print("Restoring from checkpoint path", args.checkpoint_path)
            trainer.restore(args.checkpoint_path)
        else:
            raise IOError(
                f'provided checkpoint {args.checkpoint_path} does not exist')

    # overwrite now to set a checkpoint to use going forward
    live_checkpoint_path = CHECKPOINT_FILE.format(
        args.run) if args.checkpoint_path is None else args.checkpoint_path

    print("beginning training loop with policies, observation space, action space:")
    print(multiagent_conf)
    print(obs_space)
    print(act_space)

    # Serving and training loop.
    while True:
        print(pretty_print(trainer.train()))
        checkpoint = trainer.save(checkpoint_dir=args.checkpoint_path)
        print("Last checkpoint", checkpoint)
        with open(live_checkpoint_path, "w") as f:
            f.write(checkpoint)
