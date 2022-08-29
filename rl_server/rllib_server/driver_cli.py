import argparse
from rl_server.so_routing.env.driver_policy.driver_action_space import DriverActionSpace

SERVER_BASE_PORT = 9900

parser = argparse.ArgumentParser()

parser.add_argument(
    "--port",
    type=int,
    default=SERVER_BASE_PORT,
    help="The base-port to use (on localhost). " f"Default is {SERVER_BASE_PORT}.",
)
parser.add_argument(
    "--callbacks-verbose",
    action="store_true",
    help="Activates info-messages for different events on "
    "server/client (episode steps, postprocessing, etc..).",
)
parser.add_argument(
    "--num-workers",
    type=int,
    default=0,
    help="The number of workers to use. Each worker will create "
    "its own listening socket for incoming experiences.",
)
parser.add_argument(
    "--no-restore",
    action="store_true",
    help="Do not restore from a previously saved checkpoint (location of "
    "which is saved in `last_checkpoint_[algo-name].out`).",
)
parser.add_argument(
    "--checkpoint",
    default=None,
    help="checkpoint location, or none if no checkpoint to load"
)

# General args.
parser.add_argument(
    "--run",
    default="DQN",
    choices=["APEX", "DQN", "IMPALA", "PPO", "R2D2"],
    help="The RLlib-registered algorithm to use.",
)
parser.add_argument(
    "--framework",
    choices=["tf", "tf2", "tfe", "torch"],
    default="torch",
    help="The DL framework specifier.",
)
parser.add_argument(
    "--use-lstm",
    action="store_true",
    help="Whether to auto-wrap the model with an LSTM. Only valid option for "
    "--run=[IMPALA|PPO|R2D2]",
)
parser.add_argument(
    "--stop-iters", type=int, default=6, help="Number of iterations to train."
)
parser.add_argument(
    "--stop-timesteps",
    type=int,
    default=14000,
    help="Number of timesteps to train.",
)
parser.add_argument(
    "--stop-reward",
    type=float,
    default=80.0,
    help="Reward at which we stop training.",
)
parser.add_argument(
    "--as-test",
    action="store_true",
    help="Whether this script should be run as a test: --stop-reward must "
    "be achieved within --stop-timesteps AND --stop-iters.",
)
parser.add_argument(
    "--no-tune",
    action="store_true",
    help="Run without Tune using a manual train loop instead. Here,"
    "there is no TensorBoard support.",
)
parser.add_argument(
    "--local-mode",
    action="store_true",
    help="Init Ray in local mode for easier debugging.",
)


# parser.add_argument("--run", type=str, choices=["DQN", "PPO"], default="DQN")
# parser.add_argument(
#     "--framework",
#     choices=["tf", "torch"],
#     default="torch",
#     help="The DL framework specifier.")
# parser.add_argument(
#     "--no-restore",
#     action="store_true",
#     help="Do not restore from a previously saved checkpoint (location of "
#          "which is saved in `last_checkpoint_[algo-name].out`).")
# parser.add_argument(
#     "--chatty-callbacks",
#     action="store_true",
#     help="Activates info-messages for different events on "
#          "server/client (episode steps, postprocessing, etc..).")
# parser.add_argument(
#     "--checkpoint-path",
#     help="location of checkpoint directory",
#     default=None
# )

# SO TESTBED ARGUMENTS
# parser.add_argument(
#     "--num-trips",
#     type=int,
#     help="total number of trips to expect in the simulation. a person "
#     "may have multiple trips, so this may not be the population size. "
#     "it should instead be the number of 'episodes' to expect",
#     required=True
# )
parser.add_argument(
    "--max-bid",
    type=int,
    help="maximum allowed bid to place",
    required=True
)
parser.add_argument(
    "--max-account",
    type=int,
    help="in this scenario, the maximum balance of karma "
    "any agent can have at any given point in time",
    required=True
)
parser.add_argument(
    '--action-space',
    help="type of action space to use",
    type=DriverActionSpace.argparse,
    choices=list(DriverActionSpace),
    default=DriverActionSpace.DISCRETE
)
parser.add_argument(
    '--feature-names',
    help="comma-delimited list of feature names for the observation space",
    type=str,
    required=True
)
