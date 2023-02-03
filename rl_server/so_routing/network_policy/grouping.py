from __future__ import annotations

from pathlib import Path
from typing import Dict, List
import csv
from dataclasses import dataclass


@dataclass(frozen=True)
class Grouping:
    """
    a (network policy) grouping file is a CSV, where each row represents an agent (zone).
    on the simulator side, this file is read in to provide zonal agent data. if it is a 
    polygon collection, it would contain the geometry along with an id. 
    as there are a number of ways zone ids are generated across this testbed, we assume
    that an id is an arbitrary but unique string.
    here in the RL server, we build this grouping object to translate between grid ids and
    grid indices into a spaces.Tuple representing an observation in a collaborative environment.
    """
    to_idx_lookup: Dict[str, int]
    to_id_lookup: List[str]
    n_agents: int

    def to_idx(self, s: str) -> int:
        try:
            return self.to_idx_lookup[str(s)]
        except KeyError as e:
            raise Exception(f'id {s} not found in grouping') from e

    def to_id(self, idx: int) -> str:
        try:
            return self.to_id_lookup[idx]
        except Exception as e:
            raise Exception(f'index {idx} does not correspond to an id') from e

    @classmethod
    def build(cls, grouping_file: Path, grid_id_col: str = 'grid_id') -> Grouping:
        """
        reads a grouping file and extracts the grid ids. 
        associates each grid id with a sequence index starting at zero.
        this is used to map between whatever the grid ids are (can be str/int/whatever) and
        the tuple index of an agent. QMIX requires a spaces.Tuple() environment which necessitates
        this general solution for grid_id names.
        we enforce here that all values read from the grouping file are strings.
        """
        try:
            with grouping_file.open() as f:
                ids = list(
                    map(lambda r: str(r[grid_id_col]), csv.DictReader(f)))
                to_idx = {grid_id: idx for idx, grid_id in enumerate(ids)}
                to_id = list(to_idx.values())
                n_agents = len(to_id)
                return Grouping(to_idx, to_id, n_agents)
        except Exception as e:
            raise IOError(
                f'could not read column {grid_id_col} from file {grouping_file}') from e
