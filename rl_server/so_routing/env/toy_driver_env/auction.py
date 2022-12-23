from typing import Callable, Dict, List, Tuple
from rl_server.so_routing.env.toy_driver_env.driver import Driver
from rl_server.so_routing.env.toy_driver_env.efraimidis_spirakis_sampling import reservoir_sampling
import random
import math
import copy
import json


def resolve_winner_pay_all_auction(
        auction: List[Driver],
        bids: Dict[str, int],
        network_signal: float,
        delay_fn: Callable[[Driver], Driver],
        max_balance: int) -> Tuple[List[Driver], int]:
    """
    resolve a given winner-pay-all driver auction, returning the 
    updated Drivers and the count of losers for this auction.
    """
    # for each auction, sample the # of winners/losers
    auction_size = len(auction)
    n_losers = int(math.floor(network_signal * auction_size))
    n_winners = auction_size - n_losers
    if n_losers == 0:
        return [], 0

    # pick winners via reservoir sampling, using bids as the weight vector
    a_ids, a_bids = extract_bids_by_id(auction, bids)
    winner_ids = reservoir_sampling(a_ids, a_bids, n_winners, random)

    # assign bids and delays to winners/losers
    winner_drivers = [d for d in auction if d.driver_id_str in winner_ids]
    win_bids = [bids.get(d.driver_id_str)
                for d in auction if d.driver_id_str in winner_ids]
    loser_drivers = [delay_fn(d)
                     for d in auction if d.driver_id_str not in winner_ids]

    # execute auction transactions
    winners_updated, losers_updated, overflow_pool = transact_bids(
        winner_drivers,
        loser_drivers,
        win_bids,
        max_balance)

    finalized_auction = winners_updated
    finalized_auction.extend(losers_updated)
    if overflow_pool == 0:
        # nothing more to do
        return finalized_auction, n_losers
    else:
        # redistribute the un-allocated overflow funds to all agents in a
        # round robin fashion
        return redistribute_overflow(
            finalized_auction,
            overflow_pool,
            max_balance), n_losers


def extract_bids_by_id(
        auction: List[Driver],
        bids: Dict[str, int]) -> Tuple[List[str], List[int]]:

    # grab bids for drivers in this auction
    auction_data: List[Tuple[Driver, int]] = []
    for d in auction:
        bid = bids.get(d.driver_id_str)
        if bid is None:
            msg = (
                f'driver {d.driver_id} in auction missing bid from server\n'
                f'bids: {json.dumps(bids, indent=4)}'
            )
            raise KeyError(msg)
        bid_safe = d.balance if d.balance < bid else bid
        auction_data.append((d.driver_id_str, bid_safe))

    return zip(*auction_data)


def transact_bids(
        winners: List[Driver],
        losers: List[Driver],
        win_bids: List[int],
        max_balance: int) -> Tuple[List[Driver], List[Driver], int]:
    # distribute payments for winner-pay-all auction.
    # for each winner i, pay out the i'th bid evenly across each loser j.
    # guard against exceeding the max_balance by placing extra
    # currency in an overflow pool to be uniformly redistributed later.
    winners_update = copy.deepcopy(winners)
    losers_update = copy.deepcopy(losers)

    n_winners = len(winners_update)
    n_losers = len(losers_update)
    overflow_pool = 0
    for i in range(n_winners):
        for j in range(n_losers):
            # pull latest state for winner/loser
            winner = winners_update[i]
            win_bid = win_bids[i]
            loser = losers_update[j]

            # payment p due from d1 to d2, a fraction of d1's bid
            # proportional to the number of losers to distribute to
            p = int(math.floor(float(win_bid) / float(n_losers)))
            p_trunc = winner.balance if p > winner.balance else p
            # exceeds_loser_max = (loser.balance + p) > max_balance
            if p_trunc == 0:  # or exceeds_loser_max:
                continue
            winner_updated, winner_overs = winner.update_balance(
                -p_trunc, max_balance)
            loser_updated, loser_overs = loser.update_balance(
                p_trunc, max_balance)
            overflow_pool += (winner_overs + loser_overs)
            winners_update[i] = winner_updated
            losers_update[j] = loser_updated

    return winners_update, losers_update, overflow_pool


def redistribute_overflow(
        auction: List[Driver],
        overflow_pool: int,
        max_balance: int) -> List[Driver]:
    # we have an overflow pool because the currency is bounded.
    # redistribute any funds from the overflow pool, using a round robin
    # assignment of a single currency unit to the bidders until the pool
    # is exhausted. if all agents reach their limit before the pool
    # is emptied, raise an error
    i = 0
    overflow = overflow_pool
    overflow_drivers = copy.deepcopy(auction)
    while overflow > 0:
        d = overflow_drivers[i]
        updated, overs = d.update_balance(1, max_balance)

        if overs == 0:
            # success assigning overflow increment
            overflow -= 1
            overflow_drivers[i] = updated
        else:
            # check if all members of the auction are maxxed out
            # which would be an erroneous internal state.
            # but if not, it's just that this agent is maxxed out,
            # in which case, we can just skip them.
            no_headroom = all(
                [d.balance == max_balance for d in overflow_drivers])
            if overflow > 0 and no_headroom:
                msg = (
                    f"overflow pool has {overflow} remaining but all "
                    f"agents in this auction are maxxed on balance"
                )
                raise RuntimeError(msg)

        i = (i + 1) % len(overflow_drivers)

    return overflow_drivers
