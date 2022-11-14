from __future__ import annotations
from dataclasses import dataclass
from random import Random
from typing import List
from rl_server.so_routing.env.toy_driver_env.driver import Driver


@dataclass(frozen=True)
class ToyDriverScenario:
    name: str
    free_flow_population_threshold: int
    half_speed_population_threshold: int
    half_speed_step_distance: int
    zones_of_control: int
    zones_coverage_percent: float

    @classmethod
    def build(cls, **kwargs) -> ToyDriverScenario:
        return ToyDriverScenario(
            name=kwargs['name'],
            free_flow_population_threshold=kwargs['free_flow_population_threshold'],
            half_speed_population_threshold=kwargs['free_flow_population_max'],
            half_speed_step_distance=kwargs['half_speed_step_distance'],
            zones_of_control=kwargs['zones_of_control'],
            zones_coverage_percent=kwargs['zones_coverage_percent']
        )

    def network_load_percent(self, n_active: int) -> float:
        """
        returns the piecewise function of (n_active - ff) / (half_speed - ff)
        which estimates the current network load
        """
        if n_active <= self.free_flow_population_threshold:
            return 0
        elif n_active >= self.half_speed_population_threshold:
            return 1
        else:
            numer = float(n_active) - \
                float(self.free_flow_population_threshold)
            denom = float(self.half_speed_population_threshold) - \
                float(self.free_flow_population_threshold)
            if denom == 0.0:
                return 0.0
            else:
                return numer / denom

    def create_batches(self, active_drivers: List[Driver], rng: Random) -> List[List[Driver]]:
        """
        assigns drivers to auction batches based on the
        assumption that drivers have a uniform probability of 
        intersecting with a zone of control.
        """
        bins = [[] for _ in range(self.zones_of_control)]
        for d in active_drivers:
            in_auction = rng.random() < self.zones_coverage_percent
            if in_auction:
                bin_choice = rng.randint(0, len(bins))
                bins[bin_choice].append(d)
        return bins

    def sample_delay_increment(self, n_active: int, rng: Random) -> int:
        """
        creates an instance of trip delay sampled from the scenario
        and the number of active drivers.

        delay sampling should be determined by network load via a 
        piecewise function of the population threshold inputs.
        that should then be further refined by a random gaussian sample, where
        both mean and variance are determined by the piecewise function.
        using a gaussian rng gives us random delay effects which are
        similar to real-life trip delays which follow a normal distribution.

        """
        load = self.network_load_percent(n_active)
        step_mean = self.half_speed_step_distance * load
        this_step = rng.gauss(step_mean, step_mean)
        this_step_int = int(this_step)
        print(f'this step: {this_step} or {this_step_int}')
        return this_step_int
