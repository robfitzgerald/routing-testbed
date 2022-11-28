from rl_server.so_routing.env.toy_driver_env.scenario.scenario import Scenario
from rl_server.so_routing.env.toy_driver_env.tests.test_driver import driver
from unittest import TestCase
import random
import numpy as np


def scenario(
    name='test',
    ff_thresh=100,
    jam_thresh=200,
    mean_step=10,
    mean_delay=20,
    mean_trip=100,
    std_trip=0,
    min_trip=50,
    max_trip=150,
    zones=10,
    zones_coverage_pct=0.2
) -> Scenario:
    return Scenario(
        name, ff_thresh, jam_thresh, mean_step, mean_delay,
        mean_trip, std_trip, min_trip, max_trip,
        zones, zones_coverage_pct
    )


class MockRng:

    def __init__(value):
        self.value = value

    def gauss(self, mu, sigma):
        return self.value

    def random(self):
        return self.value


class ScenarioTests(TestCase):

    def test_network_load_free_flow(self):
        s = scenario()
        lb, ub = 0, s.free_flow_drivers_threshold - 1
        active = random.randint(lb, ub)
        load = s.network_load_percent(active)
        self.assertEqual(load, 0.0)

    def test_network_load_jam(self):
        s = scenario()
        lb, ub = s.half_speed_drivers_threshold, s.half_speed_drivers_threshold * 2
        active = random.randint(lb, ub)
        load = s.network_load_percent(active)
        self.assertEqual(load, 1.0)

    def test_network_congested(self):
        s = scenario()
        lb, ub = s.free_flow_drivers_threshold + 1, s.half_speed_drivers_threshold - 1
        active = random.randint(lb, ub)
        load = s.network_load_percent(active)
        self.assertGreater(load, 0.0)
        self.assertLess(load, 1.0)

    def test_create_batches_no_active(self):
        s = scenario()
        active = []
        batches = s.create_batches(active, random)
        expected = [[] for _ in range(s.zones_of_control)]
        self.assertEqual(batches, expected)

    def test_create_batches_with_active_drivers(self):
        # this is probably a bad idea here
        s = scenario()
        n_active = 1000
        active = [driver(d_id).start_trip(0) for d_id in range(n_active)]
        batches = s.create_batches(active, random)
        in_batch = len([d for b in batches for d in b])
        expected = n_active * s.zones_coverage_percent
        self.assertAlmostEqual(in_batch, expected, delta=50)

    def test_sample_delay_free_flow(self):
        """even if we sample this many times, the delay should be zero"""
        s = scenario()
        d = driver().start_trip(0)
        max_increase = 1.0  # 100%
        n_active = s.free_flow_drivers_threshold - 1
        n_samples = 1000
        samples = []
        for _ in range(n_samples):
            delay = s.sample_delay_increment(n_active, d, max_increase, random)
            samples.append(delay)
        delay_mean = np.mean(samples)
        self.assertEqual(delay_mean, 0)

    def test_sample_delay_jam(self):
        """if we sample this many times, the delay should be non-zero"""
        s = scenario()
        d = driver().start_trip(0)
        max_increase = 1.0  # 100%
        n_samples = 1000
        samples = []
        lb = s.free_flow_drivers_threshold
        ub = s.half_speed_drivers_threshold
        for _ in range(n_samples):
            n_active = random.randint(lb, ub + 1)
            delay = s.sample_delay_increment(n_active, d, max_increase, random)
            samples.append(delay)
        delay_mean = np.mean(samples)
        self.assertNotEqual(delay_mean, 0)

    def test_sample_move_free_flow(self):
        """even if we sample this many times, the move should equal"""
        s = scenario()
        d = driver().start_trip(0)
        n_active = s.free_flow_drivers_threshold - 1
        n_samples = 1000
        samples = []
        for _ in range(n_samples):
            move = s.sample_move_distance(n_active, d, random)
            samples.append(move)
        move_mean = np.mean(samples)
        self.assertEqual(move_mean, s.mean_step_distance)

    def test_sample_move_jam(self):
        """
        even if we sample this many times, the move should equal
        the mean step distance exactly
        """
        s = scenario()
        d = driver().start_trip(0)
        n_samples = 1000
        samples = []
        lb = s.free_flow_drivers_threshold
        ub = s.half_speed_drivers_threshold
        for _ in range(n_samples):
            n_active = random.randint(lb, ub + 1)
            move = s.sample_move_distance(n_active, d, random)
            samples.append(move)
        move_mean = np.mean(samples)
        self.assertLess(move_mean, s.mean_step_distance)
