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


import json
import os
import random
import time

import ray
from ray import air, tune
from ray.rllib.algorithms.registry import get_algorithm_class
from rl_server.rllib_server.driver_cli import parser

from ray.rllib.examples.custom_metrics_and_callbacks import MyCallbacks
from ray.rllib.models.catalog import MODEL_DEFAULTS
from ray.tune.logger import pretty_print
from ray.rllib.policy.policy import PolicySpec
from rl_server.so_routing.env.toy_driver_env.scenario.lafayette import Lafayette
from rl_server.so_routing.env.toy_driver_env.scenario.test_scenario import TestScenario

# from ray.rllib.env.policy_server_input import PolicyServerInput
from rl_server.so_routing.policy_server_no_pickle_v4 import PolicyServerInput
from rl_server.so_routing.driver_policy.driver_obs_space import DriverObsSpace, build_observation_space
from rl_server.so_routing.env.toy_driver_env.toy_driver_environment import ToyDriverEnv
from rl_server.so_routing.env.toy_driver_env.network_signal import NetworkSignalFunctionType
from rl_server.so_routing.env.toy_driver_env.observation import compose_observation_fn

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

    # same as above but +1 on port
    def _eval_input(ioctx):
        # We are remote worker or we are local worker with num_workers=0:
        # Create a PolicyServerInput.
        if ioctx.worker_index > 0 or ioctx.worker.num_workers == 0:
            return PolicyServerInput(
                ioctx,
                SERVER_ADDRESS,
                args.port + ioctx.worker_index -
                (1 if ioctx.worker_index > 0 else 0)+1,
            )
        # No InputReader (PolicyServerInput) needed.
        else:
            return None

    try:
        feature_list = args.feature_names.split(',')
        o_names = list(map(lambda s: DriverObsSpace[s], feature_list))
    except Exception as e:
        raise Exception(f"failed parsing observation features") from e

    grouping = None
    agents_list = None
    if args.grouping_file is not None:
        with open(args.grouping_file, 'r') as f:
            grouping = json.loads(f.read())
            agents_list = [agent for group in grouping.values()
                           for agent in group]
        print(
            f'grouping file with {len(agents_list)} agents in {len(grouping)} groups')

    obs_space = build_observation_space(o_names, args.max_account, agents_list)
    act_space = args.action_space.action_space(args.max_bid, agents_list)
    # is_multi = grouping is not None
    # obs_space_instance = list(obs_space.values())[0] if is_multi else obs_space
    # act_space_instance = list(act_space.values())[0] if is_multi else act_space

    print(f"observation space of type {type(obs_space)}:")
    print(obs_space)
    print(f"action space of type {type(act_space)}")
    print(act_space)

    multiagent = {
        # Map of type MultiAgentPolicyConfigDict from policy ids to tuples
        # of (policy_cls, obs_space, act_space, config). This defines the
        # observation and action spaces of the policies and any extra config.
        "policies": {
            "driver": PolicySpec(None, obs_space, act_space),
        },
        # Keep this many policies in the "policy_map" (before writing
        # least-recently used ones to disk/S3).
        # "policy_map_capacity": 100,
        # Where to store overflowing (least-recently used) policies?
        # Could be a directory (str) or an S3 location. None for using
        # the default output dir.
        # "policy_map_cache": None,
        # Function mapping agent ids to policy ids.
        # "policy_mapping_fn": None,
        "policy_mapping_fn": lambda agent_id, episode, worker, **kwargs: "driver",
        # Determines those policies that should be updated.
        # Options are:
        # - None, for all policies.
        # - An iterable of PolicyIDs that should be updated.
        # - A callable, taking a PolicyID and a SampleBatch or MultiAgentBatch
        #   and returning a bool (indicating whether the given policy is trainable
        #   or not, given the particular batch). This allows you to have a policy
        #   trained only on certain data (e.g. when playing against a certain
        #   opponent).
        # "policies_to_train": None,
        # Optional function that can be used to enhance the local agent
        # observations to include more state.
        # See rllib/evaluation/observation_function.py for more info.
        # "observation_fn": None,
        # When replay_mode=lockstep, RLlib will replay all the agent
        # transitions at a particular timestep together in a batch. This allows
        # the policy to implement differentiable shared computations between
        # agents it controls at that timestep. When replay_mode=independent,
        # transitions are replayed independently per policy.
        # "replay_mode": "independent",
        # Which metric to use as the "batch size" when building a
        # MultiAgentBatch. The two supported values are:
        # env_steps: Count each time the env is "stepped" (no matter how many
        #   multi-agent actions are passed/how many multi-agent observations
        #   have been returned in the previous step).
        # agent_steps: Count each individual agent step as one step.
        "count_steps_by": "agent_steps",
    }

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
        # Divide episodes into fragments of this many steps each during rollouts.
        # Sample batches of this size are collected from rollout workers and
        # combined into a larger batch of `train_batch_size` for learning.
        #
        # For example, given rollout_fragment_length=100 and train_batch_size=1000:
        #   1. RLlib collects 10 fragments of 100 steps each from rollout workers.
        #   2. These fragments are concatenated and we perform an epoch of SGD.
        #
        # When using multiple envs per worker, the fragment size is multiplied by
        # `num_envs_per_worker`. This is since we are collecting steps from
        # multiple envs in parallel. For example, if num_envs_per_worker=5, then
        # rollout workers will return experiences in chunks of 5*100 = 500 steps.
        #
        # The dataflow here can vary per algorithm. For example, PPO further
        # divides the train batch into minibatches for multi-epoch SGD.
        "rollout_fragment_length": 200,

        # Training batch size, if applicable. Should be >= rollout_fragment_length.
        # Samples batches will be concatenated together to a batch of this size,
        # which is then passed to SGD.

        # RJF: 8000 agents, 2 trips, 20% adoption, avg 7 replannings -> 22,400
        # agent observation/action/reward tuples to train from?
        "train_batch_size": 1000,

        # Number of environments to evaluate vector-wise per worker. This enables
        # model inference batching, which can improve performance for inference
        # bottlenecked workloads.
        # "num_envs_per_worker": 0,
        # Disable OPE, since the rollouts are coming from online clients.
        "off_policy_estimation_methods": {},
        # Create a "chatty" client/server or not.
        "callbacks": MyCallbacks if args.callbacks_verbose else None,
        # DL framework to use.
        "framework": args.framework,
        # Set to INFO so we'll see the server's actual address:port.
        "log_level": "DEBUG",
        "model": {},

        # === Evaluation Settings ===
        # Evaluate with every `evaluation_interval` training iterations.
        # The evaluation stats will be reported under the "evaluation" metric key.
        # Note that for Ape-X metrics are already only reported for the lowest
        # epsilon workers (least random workers).
        # Set to None (or 0) for no evaluation.
        # "evaluation_interval": 10,
        # Duration for which to run evaluation each `evaluation_interval`.
        # The unit for the duration can be set via `evaluation_duration_unit` to
        # either "episodes" (default) or "timesteps".
        # If using multiple evaluation workers (evaluation_num_workers > 1),
        # the load to run will be split amongst these.
        # If the value is "auto":
        # - For `evaluation_parallel_to_training=True`: Will run as many
        #   episodes/timesteps that fit into the (parallel) training step.
        # - For `evaluation_parallel_to_training=False`: Error.
        # "evaluation_duration": 1,
        # The unit, with which to count the evaluation duration. Either "episodes"
        # (default) or "timesteps".
        # "evaluation_duration_unit": "episodes",
        # "evaluation_config": {
        #     "input": _eval_input,
        #     "observation_space": obs_space,
        #     "action_space": act_space,
        #     "multiagent": multiagent
        # },
        "multiagent": multiagent
    }

    # DQN settings if we are training
    if not args.as_test:
        # Example of using DQN (supports off-policy actions).
        config.update(
            {
                "explore": not args.as_test,
                "exploration_config": {
                    # Exploration sub-class by name or full path to module+class
                    # (e.g. “ray.rllib.utils.exploration.epsilon_greedy.EpsilonGreedy”)
                    "type": "EpsilonGreedy",
                    # Parameters for the Exploration class' constructor:
                    "initial_epsilon": 1.0,
                    "final_epsilon": 0.02,
                    "warmup_timesteps": 14000,
                    # (agent) Timesteps over which to anneal epsilon.
                    #
                    "epsilon_timesteps": 140000,
                }
            }
        )
        # config["model"] = {
        #     "fcnet_hiddens": [64],
        #     "fcnet_activation": "linear",
        # }
        if args.run == "R2D2":
            config["model"]["use_lstm"] = args.use_lstm

    if args.toy:
        # override config for toy problem setup
        config["input"] = "sampler"
        config['env'] = ToyDriverEnv
        config['model'] = MODEL_DEFAULTS
        # config['model']["use_attention"] = True

        # def population_fn(iteration):
        #     min_pop = 100
        #     max_pop = 1000

        config['env_config'] = {
            "scenario": Lafayette,  # TestScenario,
            "seed": args.seed,
            "max_balance": args.max_account,
            "population_fn": lambda i: random.randint(1000, 9000),
            "network_signal_fn": NetworkSignalFunctionType.THRESH_TEN_DRIVERS.create_fn(),
            "observation_fn": compose_observation_fn(o_names, args.max_account),
            "observation_space": obs_space,
            "action_space": act_space,
            "log_filename": "log.csv"
        }

        # config['env_config'] = {
        #     "scenario": TestScenario,  # TestScenario,
        #     "seed": args.seed,
        #     "max_balance": args.max_account,
        #     "population_fn": lambda i: random.randint(50, 300),
        #     "network_signal_fn": NetworkSignalFunctionType.THRESH_FIVE_DRIVERS.create_fn(),
        #     "observation_fn": compose_observation_fn(o_names, args.max_account),
        #     "observation_space": obs_space,
        #     "action_space": act_space,
        #     "log_filename": "log.csv"
        # }

        ser_conf = config.copy()
        for k, v in ser_conf.items():
            ser_conf[k] = str(v)
        print("configuration:")
        print(json.dumps(ser_conf, indent=4))

        algo = get_algorithm_class(args.run)(config=config)
        if args.checkpoint_path:
            print("Restoring from checkpoint path", args.checkpoint_path)
            algo.restore(args.checkpoint_path)

        if args.as_test:
            # create a test configuration
            test_config = config.copy()
            test_config.update({"explore": False})

            algo = get_algorithm_class(args.run)(config=test_config)
            results = algo.train()
            print(results)

        else:
            iterations = range(algo.iteration, args.stop_iters + 1)
            print(f"running {len(iterations)} iterations")
            for i in iterations:
                print(f"running driver server training iteration {i}")

                results = algo.train()
                save_checkpoint = i != 0 and i % args.checkpoint_interval == 0
                if save_checkpoint:
                    checkpoint = algo.save()

                met_reward_condition = results["policy_reward_mean"]["driver"] >= args.stop_reward
                met_ts_condition = ts >= args.stop_timesteps if args.stop_timesteps is not None else False
                if met_reward_condition or met_ts_condition:
                    break

            checkpoint = algo.save()
            print("Last checkpoint", checkpoint)

    # Manual training loop (no Ray tune), allows for using checkpoint
    elif args.no_tune:
        algo = get_algorithm_class(args.run)(config=config)

        if args.checkpoint_path:
            print("Restoring from checkpoint path", args.checkpoint_path)
            algo.restore(args.checkpoint_path)

        if args.as_test:
            # run until user terminates process
            while True:
                time.sleep(0.2)
                pass
        else:
            # Serving and training loop.
            ts = 0
            for i in range(algo.iteration, args.stop_iters + 1):

                print(f"begin training iteration #{i}")
                results = algo.train()

                print(f'results for training iteration #{i}')
                print(pretty_print(results))

                print(f"saving checkingpoint for iteration #{i}")
                checkpoint = algo.save()
                print("Last checkpoint", checkpoint)

                ts += results["timesteps_total"]
                met_reward_condition = results["policy_reward_mean"]["driver"] >= args.stop_reward
                met_ts_condition = ts >= args.stop_timesteps if args.stop_timesteps is not None else False
                print(f"met reward stopping condition? {met_reward_condition}")
                if args.stop_timesteps is not None:
                    print(
                        f"met timestep stopping condition? {met_ts_condition}")

                print(f"end of training iteration #{i}")
                if met_reward_condition or met_ts_condition:
                    break

            print('finished training, terminating server.')

    # Run with Tune for auto env and trainer creation and TensorBoard.
    else:
        print("Ignoring restore even if previous checkpoint is provided...")
        stop = {
            "training_iteration": args.stop_iters,
            "timesteps_total": args.stop_timesteps,
            # "episode_reward_mean": args.stop_reward,
        }

        tune.Tuner(
            args.run,
            param_space=config,
            run_config=air.RunConfig(stop=stop, verbose=2)
        ).fit()
