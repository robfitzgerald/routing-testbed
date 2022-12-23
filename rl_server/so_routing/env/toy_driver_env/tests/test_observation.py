from rl_server.so_routing.env.toy_driver_env.driver import Driver, AgentType, DriverState
import rl_server.so_routing.env.toy_driver_env.observation as rseto
from rl_server.so_routing.env.toy_driver_env.reward import jain_index
from unittest import TestCase
import random
import math
import numpy as np


class TestObservation(TestCase):

    def test_batch_fairness(self):
        # this is just a driver application for reviewing the fairness externalities score
        # under different random conditions
        n_agents = 10
        for w in range(100):

            # reroute = random.randint(0, int(math.floor(w / 5)))
            reroute = random.randint(0, w)

            batch = []
            delays = []
            for _ in range(n_agents):
                original_trip = 20
                prev_reroute = random.randint(0, reroute)
                prev_congestion = random.randint(0, 5)
                d = Driver.build(0, AgentType.PARTICIPANT,
                                 50, 0, original_trip, 0).reroute(prev_reroute, 0).add_congestion_delay(prev_congestion)
                this_delay = random.randint(0, reroute)
                batch.append(d)
                delays.append(this_delay)
            winners = random.randint(0, n_agents)
            result, lb, ub = rseto.estimate_batch_fairness_externalities(
                batch, delays, winners)

            ub_vals = [d.trip_total() for d in batch]
            lb_vals = [a + b for a, b in zip(ub_vals, delays)]
            ub_m, ub_s = np.mean(ub_vals), np.std(ub_vals)
            lb_m, lb_s = np.mean(lb_vals), np.std(lb_vals)
            prev_delays = [d.overall_delay() for d in batch]
            jain_batch = jain_index([1 - d.overall_delay_pct() for d in batch])
            print(
                f'winners {str(winners).ljust(2)} jain: {jain_batch:.4f} externalities: {result:.4f} (~ {ub:.4f} - {lb:.4f}) -- '
                f'ub {ub_vals} lb {lb_vals} ub_µ {ub_m:.1f} ub_s {ub_s:.2f} lb_µ {lb_m:.1f} lb_s {lb_s:.2f}'
                # f'(prev_delays: {prev_delays} next_delays: {delays})'
            )
