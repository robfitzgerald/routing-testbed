import random
from unittest import TestCase
import rl_server.so_routing.env.toy_driver_env.auction as rseta
from rl_server.so_routing.env.toy_driver_env.tests.test_driver import driver


class AuctionTest(TestCase):

    def test_extract_bids_by_id(self):
        # extracting bids should create two lists, one with
        # the ids, and one with bids, where the (common) index
        # of both lists references a single driver/bid tuple
        drivers = [driver(i) for i in range(10)]
        random.shuffle(drivers)
        bids = {d.driver_id_str: i for i, d in enumerate(drivers)}

        ds_ordered, bids_ordered = rseta.extract_bids_by_id(drivers, bids)
        for idx, d_id in enumerate(ds_ordered):
            orig_bid = bids.get(d_id)
            ordered_bid = bids_ordered[idx]
            self.assertEqual(orig_bid, ordered_bid)

    def test_transact_bids_empty(self):
        winners = []
        losers = []
        win_bids = []
        max_balance = 100
        win_result, lose_result, overflow = rseta.transact_bids(
            winners,
            losers,
            win_bids,
            max_balance)
        self.assertEqual(win_result, [])
        self.assertEqual(lose_result, [])
        self.assertEqual(overflow, 0)

    def test_transact_bids_no_losers(self):
        winners = [driver(i) for i in range(0, 4)]
        losers = []
        win_bids = [i for i in range(0, 4)]
        max_balance = 100
        win_result, lose_result, overflow = rseta.transact_bids(
            winners,
            losers,
            win_bids,
            max_balance)
        self.assertEqual(winners, win_result)
        self.assertEqual(lose_result, [])
        self.assertEqual(overflow, 0)

    def test_transact_bids_no_overflow(self):
        """
        test relies on symmetry between # bid values and # of losers
        """
        winners = [driver(i) for i in range(0, 5)]
        losers = [driver(i) for i in range(5, 10)]
        win_bids = [i for i in range(5, 30, 5)]
        max_balance = 100
        win_result, lose_result, overflow = rseta.transact_bids(
            winners,
            losers,
            win_bids,
            max_balance)
        for winner in win_result:
            bid = win_bids[winner.driver_id]
            self.assertEqual(winner.balance, winner.original_balance - bid)
        for loser in lose_result:
            self.assertGreater(loser.balance, loser.original_balance)
        self.assertEqual(overflow, 0)

    def test_transact_bids_with_overflow(self):
        """
        if all agents are at max_balance, then all bids should
        end up in the overflow_pool
        """
        winners = [driver(i) for i in range(0, 5)]
        losers = [driver(i) for i in range(5, 10)]
        win_bids = [i for i in range(5, 30, 5)]
        max_balance = 50
        win_result, lose_result, overflow = rseta.transact_bids(
            winners,
            losers,
            win_bids,
            max_balance)
        for winner in win_result:
            bid = win_bids[winner.driver_id]
            self.assertEqual(winner.balance, winner.original_balance - bid)
        for loser in lose_result:
            self.assertEqual(loser.balance, loser.original_balance)
        self.assertEqual(overflow, sum(win_bids))

    def test_redistribute_with_headroom(self):
        n_drivers = 10
        overflow_pool = 50
        expected_increment = overflow_pool / n_drivers
        max_balance = 100
        auction = [driver(i) for i in range(n_drivers)]
        result = rseta.redistribute_overflow(
            auction,
            overflow_pool,
            max_balance)
        for d in result:
            expected = d.original_balance + expected_increment
            self.assertEqual(d.balance, expected)

    def test_redistribute_without_headroom(self):
        n_drivers = 10
        overflow_pool = 50
        max_balance = 50
        auction = [driver(i) for i in range(n_drivers)]
        with self.assertRaises(RuntimeError):
            rseta.redistribute_overflow(
                auction,
                overflow_pool,
                max_balance)
