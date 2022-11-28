from rl_server.so_routing.env.toy_driver_env.driver import Driver, DriverState
from unittest import TestCase


def driver(
    driver_id=0,
    balance=50,
    trip_start_time=0,
    original_trip_total=10,
    trip_position=0,
    delay=0,
    replannings=0,
    most_recent_replanning_time=None,
    state=DriverState.INACTIVE
) -> Driver:
    return Driver(
        driver_id, balance, balance, trip_start_time,
        original_trip_total, trip_position, delay,
        replannings, most_recent_replanning_time, state)


class DriverTest(TestCase):

    def test_start_trip(self):
        d0 = driver()
        d1 = d0.start_trip(0)
        self.assertEqual(d1.state, DriverState.ACTIVE)
        self.assertEqual(d1.most_recent_route_assignment_time, 0)

    def test_update_if_arrived_when_not_arrived(self):
        d0 = driver(trip_position=0, original_trip_total=5,
                    state=DriverState.ACTIVE)
        d1 = d0.update_if_arrived()
        self.assertEqual(d0, d1)

    def test_update_if_arrived_when_arrived(self):
        d0 = driver(trip_position=5, original_trip_total=5,
                    state=DriverState.ACTIVE)
        d1 = d0.update_if_arrived()
        self.assertEqual(d1.state, DriverState.ARRIVED)

    def test_move_zero(self):
        d0 = driver().start_trip(0)
        d1 = d0.move(0)
        self.assertEqual(d0, d1)

    def test_move_increment(self):
        increment = 2
        d0 = driver().start_trip(0)
        d1 = d0.move(increment)
        self.assertEqual(d1.trip_position, increment)

    def test_move_exceeds_remaining(self):
        increment = 5
        d0 = driver(trip_position=6, original_trip_total=10,
                    state=DriverState.ACTIVE)
        d1 = d0.move(increment)
        self.assertEqual(d1.trip_position, d1.original_trip_total)

    def test_reroute_zero(self):
        delay = 0
        current_time = 1
        d0 = driver().start_trip(0)
        d1 = d0.reroute(delay, current_time)
        self.assertEqual(d1.replannings, 1)
        self.assertEqual(d0.trip_position, d1.trip_position)
        self.assertEqual(d0.trip_total(), d1.trip_total())

    def test_reroute_increment(self):
        delay = 4
        current_time = 0
        d0 = driver(original_trip_total=6).start_trip(0)
        d1 = d0.reroute(delay, current_time)
        # 6 + 4 = 10
        self.assertEqual(d1.trip_total(), 10)
        # 0 / (6 + 4) = 0
        self.assertEqual(d1.trip_pct(), 0)
        # 4/10 = 0.4
        self.assertEqual(d1.delay_pct(), 0.4)
        # (10-6)/6 = 0.6666...
        self.assertAlmostEqual(d1.delay_offset_pct(), 0.6667, places=4)
        # 6 / 10 = 0.6
        self.assertEqual(d1.pct_original_of_final(), 0.6)

    def test_update_balance_pos(self):
        max_balance = 100
        increment = 5
        d0 = driver().start_trip(0)
        d1, overflow = d0.update_balance(increment, max_balance)
        self.assertEqual(d1.balance, d0.balance + increment)
        self.assertEqual(overflow, 0)

    def test_update_balance_neg(self):
        max_balance = 100
        increment = -5
        d0 = driver().start_trip(0)
        d1, overflow = d0.update_balance(increment, max_balance)
        self.assertEqual(d1.balance, d0.balance + increment)
        self.assertEqual(overflow, 0)

    def test_update_balance_overflow(self):
        max_balance = 100
        increment = 55
        d0 = driver().start_trip(0)
        d1, overflow = d0.update_balance(increment, max_balance)
        self.assertEqual(d1.balance, max_balance)
        self.assertEqual(overflow, (d0.balance + increment) % max_balance)

    def test_update_balance_underflow(self):
        max_balance = 100
        increment = -55
        d0 = driver().start_trip(0)
        with self.assertRaises(ValueError):
            d0.update_balance(increment, max_balance)
