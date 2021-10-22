from typing import List

from gym import Space
from gym.spaces import Box, Discrete, Tuple, Dict
import numpy as np

# mimic the space design as implemented in the Scala code
# edu.colorado.fitzgero.sotestbed.algorithm.selection.rl.SpaceV1

# EPSG:3857 (Web Mercator Projection) has the following coordinate bounds:
X_MIN = 0-20026376.39
Y_MIN = 0-20048966.10
X_MAX = 20026376.39
Y_MAX = 20048966.10

observation_space = Box(
    low=np.array([X_MIN, Y_MIN, X_MIN, Y_MIN, np.NINF]),
    high=np.array([X_MAX, Y_MAX, X_MAX, Y_MAX, np.Inf]),
    shape=(5,),
    dtype=np.float64
)


def create_space(n_alts: int) -> (Space, Space):
    """
    creates the multi-agent (Observation, Action) spaces given this set of agent ids
    which are grouped by their grouping ids

    :return: the observation and action spaces for this grouping
    """

    return observation_space, Discrete(n_alts)
