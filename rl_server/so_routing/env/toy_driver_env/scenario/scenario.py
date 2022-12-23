from __future__ import annotations
from dataclasses import dataclass, asdict
from random import Random
from typing import List
from rl_server.so_routing.env.toy_driver_env.driver import Driver
import numpy as np


@dataclass(frozen=True)
class Scenario:
    name: str
    adoption_rate: float
    min_start_time: int
    max_start_time: int
    max_replannings: int
    max_trip_increase_pct: float
    free_flow_drivers_threshold: int
    half_speed_drivers_threshold: int
    mean_step_distance: int
    mean_congestion_delay: int
    mean_replanning_delay: int
    mean_trip_distance: int
    stdev_trip_distance: int
    min_trip_distance: int
    max_trip_distance: int
    zones_of_control: int
    zones_coverage_percent: float

    def as_dict(self):
        return asdict(self)

    def network_load_percent(self, n_active: int) -> float:
        """
        returns the piecewise function of (n_active - ff) / (half_speed - ff)
        which estimates the current network load
        """
        if n_active <= self.free_flow_drivers_threshold:
            return 0
        elif n_active >= self.half_speed_drivers_threshold:
            return 1
        else:
            ff_thresh = float(self.free_flow_drivers_threshold)
            numer = float(n_active) - ff_thresh
            denom = float(self.half_speed_drivers_threshold) - ff_thresh
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
                # let's sample from a binomial distribution, which
                # will give us a "spike downtown" effect over the
                # possible zones
                binom_result = np.random.binomial(
                    n=len(bins) - 1, p=0.5, size=1)
                bin_choice = binom_result[0]
                bins[bin_choice].append(d)
        return bins

    def sample_initial_trip_distance(self, rng: Random) -> int:
        trip_distance = rng.gauss(
            self.mean_trip_distance,
            self.stdev_trip_distance)
        trip_result = min(self.max_trip_distance, max(
            self.min_trip_distance, trip_distance))
        return trip_result

    def sample_replanning_delay(
        self,
        n_active: int,
        driver: Driver,
        max_trip_increase: float
    ) -> int:
        return self.sample_delay_increment(n_active, driver, max_trip_increase, self.mean_replanning_delay)

    def sample_congestion_delay(
        self,
        n_active: int,
        driver: Driver,
        max_trip_increase: float
    ) -> int:
        return self.sample_delay_increment(n_active, driver, max_trip_increase, self.mean_congestion_delay)

    def sample_delay_increment(
            self,
            n_active: int,
            driver: Driver,
            max_trip_increase: float,
            mean_delay: int) -> int:
        """
        creates an instance of trip delay sampled from the scenario
        and the number of active drivers.

        delay sampling should be determined by network load, computed via a 
        piecewise function of the population threshold inputs.
        that should then be further refined by a random gaussian sample, where
        both mean and variance are determined by the network load percentage,
        multiplied against the provided mean trip delay set by the simulation.

        using a gaussian rng gives us random delay effects which are
        similar to real-life trip delays which follow a normal distribution.

        """
        # how much delay could this driver be given?
        delay_headroom = driver.remaining_delay_headroom(max_trip_increase)
        if delay_headroom == 0:
            # msg = (
            #     f'DELAY driver {driver.driver_id} delay: 0 (driver has '
            #     f'reached max trip increase of {max_trip_increase*100:.2f}% '
            #     f'or {driver.trip_total()} meters)'
            # )
            # print(msg)
            return 0
        load = self.network_load_percent(n_active)
        luck_effect = load - driver.luck  # pos. luck should "reduce" load effect
        del_dist = int(mean_delay * luck_effect)
        delay_result = min(delay_headroom, max(0, del_dist))
        # msg = (
        #     f'DELAY driver {driver.driver_id_str.ljust(4)} delay: {str(delay_result).ljust(5)} '
        #     f'(load: {load} luck: {driver.luck:.2f}; luck effect: {luck_effect:.4f} delay sample: {del_dist} '
        #     f'delay result: {delay_result} n_active {n_active} headroom for trip increase: {delay_headroom:.2f} meters '
        #     f'{(max_trip_increase*100)-(driver.replanning_delay_offset_pct()*100):.2f}%)'
        # )
        # print(msg)
        return delay_result

    def sample_move_distance(self, n_active: int, driver: Driver, rng: Random) -> int:
        """
        move a driver. this should be based on the number of
        active drivers but fairly consistent for all so we don't add
        in too much noise.
        """
        remaining_trip = driver.trip_remaining()
        # load = self.network_load_percent(n_active)
        # move_mean = 1.0 - load
        # move_std = load
        # move_norm = rng.gauss(move_mean, move_std)
        # move_dist = int(move_norm * self.mean_step_distance)
        # move_result = int(min(remaining_trip, max(0, move_dist)))
        # remaining_after_move = max(0, move_result - remaining_trip)
        # msg = (
        #     f'MOVE driver {driver.driver_id} dist: {move_result} (load: {load} '
        #     f'samp norm: {move_norm:.4f} samp dist: {move_dist} n_active {n_active} '
        #     f'remaining after move: {remaining_after_move})'
        # )
        # print(msg)
        move_result = int(min(remaining_trip, max(0, self.mean_step_distance)))
        return move_result
