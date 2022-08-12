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
import json

import ray
from ray import tune
from ray.rllib.agents.registry import get_trainer_class
from ray.tune.logger import pretty_print
# from ray.rllib.utils.tf_utils import get_gpu_devices
# from ray.rllib.env.external_multi_agent_env import ExternalEnv

# from rl_server.so_routing.util.torch_utils import run_torch_benchmark
from rl_server.so_routing.env.policy_server_no_pickle_v2 import PolicyServerNoPickleInput
from rl_server.so_routing.env.driver_policy.driver_space import DriverSpaceEncoder
from rl_server.rllib_server.conf import server_conf
from rl_server.rllib_server.driver_cli import parser
from ray.rllib.examples.custom_metrics_and_callbacks import MyCallbacks


conf = server_conf("driver_policy")
SERVER_ADDRESS = conf['host']
# In this example, the user can run the policy server with
# n workers, opening up listen ports 9900 - 990n (n = num_workers - 1)
# to each of which different clients may connect.
SERVER_BASE_PORT = conf['port']  # + worker-idx - 1

print(f'host: {SERVER_ADDRESS} base port: {SERVER_BASE_PORT}')

CHECKPOINT_FILE = "last_checkpoint_{}.out"


def run():
    args = parser.parse_args()
    ray.init()  # num_cpus=1?
    print(f'ray version {ray.__version__}')
    print(json.dumps(vars(args), indent=4, cls=DriverSpaceEncoder))

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
            )
        # No InputReader (PolicyServerInput) needed.
        else:
            return None

    # if 'tf' in args.framework:
    #     print(f"GPU Devices: {get_gpu_devices()}")
    # run_torch_benchmark()

    # build the observation and action spaces for this run
    obs_space = args.driver_space.observation_space(args.max_account)
    act_space = args.driver_space.action_space(args.max_bid)
    # env = ExternalEnv(act_space, obs_space, args.num_trips)
    print("OBSERVATION SPACE:")
    print(obs_space)
    print("ACTION SPACE:")
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
        # Use n worker processes to listen on different ports.
        "num_workers": args.num_workers,
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
    if args.run == "DQN":
        # Example of using DQN (supports off-policy actions).
        config.update(
            {
                "replay_buffer_config": {"learning_starts": 100},
                "timesteps_per_iteration": 200,
                "n_step": 3,
                "rollout_fragment_length": 4,
                "train_batch_size": 8,
            }
        )
        config["model"] = {
            "fcnet_hiddens": [64],
            "fcnet_activation": "linear",
        }

    # QMIX.
    elif args.run == "QMIX":
        config.update(
            {
                # "num_envs_per_worker": 5,  # test with vectorization on
                "mixer": "qmix",
                # "framework": args.framework,  # only "torch" allowed here
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
            }
        )

    # PPO.
    else:
        # Example of using PPO (does NOT support off-policy actions).
        config.update(
            {
                "rollout_fragment_length": 1000,
                "train_batch_size": 4000
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
        algo_cls = get_trainer_class(args.run)
        algo = algo_cls(config=config)

        if checkpoint_path:
            print("Restoring from checkpoint path", checkpoint_path)
            algo.restore(checkpoint_path)

        # Serving and training loop.
        ts = 0
        for _ in range(args.stop_iters):
            results = algo.train()
            print(pretty_print(results))
            checkpoint = algo.save()
            print("Last checkpoint", checkpoint)
            with open(checkpoint_path, "w") as f:
                f.write(checkpoint)
            if (
                results["episode_reward_mean"] >= args.stop_reward
                or ts >= args.stop_timesteps
            ):
                break
            ts += results["timesteps_total"]

    # Run with Tune for auto env and algo creation and TensorBoard.
    else:
        stop = {
            "training_iteration": args.stop_iters,
            "timesteps_total": args.stop_timesteps,
            "episode_reward_mean": args.stop_reward,
        }

        tune.run(args.run, config=config, stop=stop,
                 verbose=2, restore=checkpoint_path)

    # # Attempt to restore from checkpoint, if possible.
    # if not args.no_restore and args.checkpoint_path is not None:
    #     if os.path.exists(args.checkpoint_path):
    #         # checkpoint_path = open(checkpoint_path).read()
    #         print("Restoring from checkpoint path", args.checkpoint_path)
    #         trainer.restore(args.checkpoint_path)
    #     else:
    #         raise IOError(
    #             f'provided checkpoint {args.checkpoint_path} does not exist')

    # # overwrite now to set a checkpoint to use going forward
    # live_checkpoint_path = CHECKPOINT_FILE.format(
    #     args.run) if args.checkpoint_path is None else args.checkpoint_path

    # print(f"checkpoint path: {live_checkpoint_path}")
    # print("beginning training loop")

    # # Serving and training loop.
    # while True:
    #     print(pretty_print(trainer.train()))
    #     checkpoint = trainer.save(checkpoint_dir=args.checkpoint_path)
    #     print("Last checkpoint", checkpoint)
    #     with open(live_checkpoint_path, "w") as f:
    #         f.write(checkpoint)


if __name__ == "__main__":
    run()
