from __future__ import annotations
from enum import Enum
from dataclasses import dataclass, replace, asdict
from typing import Optional, Tuple


class DriverState(Enum):
    INACTIVE = 0
    ACTIVE = 1
    ARRIVED = 2
    DONE = 3


@dataclass(frozen=True)
class Driver:
    driver_id: int
    balance: int
    original_balance: int
    trip_start_time: int
    original_trip_total: int
    trip_position: int = 0
    delay: int = 0
    replannings: int = 0
    most_recent_route_assignment_time: Optional[int] = None
    state: DriverState = DriverState.INACTIVE

    @classmethod
    def build(cls, d_id, bal, start_time, trip_total) -> Driver:
        return cls(
            driver_id=d_id,
            balance=bal,
            original_balance=bal,
            trip_start_time=start_time,
            original_trip_total=trip_total,
            trip_position=0,
            delay=0,
            replannings=0,
            most_recent_route_assignment_time=None,
            state=DriverState.INACTIVE)

    def as_dict(self):
        return asdict(self)

    @property
    def driver_id_str(self) -> str:
        return str(self.driver_id)

    ###################
    ### DRIVER STATE ###
    @property
    def inactive(self):
        return self.state == DriverState.INACTIVE

    @property
    def active(self):
        return self.state == DriverState.ACTIVE

    @property
    def arrived(self):
        return self.state == DriverState.ARRIVED

    @property
    def done(self):
        return self.state == DriverState.DONE

     ####################################
    ### TRIP STATISTICS AND REPORTING ###
    def trip_total(self) -> int:
        """total distance of a trip including delay"""
        return self.original_trip_total + self.delay

    def trip_remaining(self) -> int:
        """remaining distance of the trip to cover"""
        return int(self.trip_total() - self.trip_position)

    def trip_pct(self) -> float:
        """percent of trip (original + delay) that has been traversed"""
        return float(self.trip_position) / float(self.trip_total())

    def delay_pct(self) -> float:
        """percent of the total trip which is due to delay"""
        return float(self.delay) / float(self.trip_total())

    def delay_offset_pct(self) -> float:
        """increase in the trip distance due to delay (percent)"""
        numer = float(self.trip_total() - self.original_trip_total)
        denom = float(self.original_trip_total)
        return numer / denom

    def remaining_delay_headroom(self, max_increase_pct: float) -> int:
        """
        returns the distance which can be added to this trip, bounded
        by the max increase percent
        """
        max_dist = self.original_trip_total * (1 + max_increase_pct)
        remaining = max(0, max_dist - self.trip_total())
        return remaining

    def pct_original_of_final(self) -> float:
        return float(self.original_trip_total) / float(self.trip_total())

    def balance_diff(self) -> int:
        return self.original_balance - self.balance

    def observation(self):
        pct_finished = self.trip_pct()
        pct_delay = self.delay_pct()
        return [self.balance, pct_finished, pct_delay]

     ####################
    ### STATE UPDATES ###
    def start_trip(self, current_time: int) -> Driver:
        updated = replace(
            self,
            state=DriverState.ACTIVE,
            most_recent_route_assignment_time=current_time
        )
        return updated

    def update_if_arrived(self):
        if self.trip_remaining() == 0:
            return replace(self, state=DriverState.ARRIVED)
        else:
            return self

    def report_done(self) -> Driver:
        return replace(self, state=DriverState.DONE)

    def move(self, distance):
        next_pos = min(self.trip_total(), self.trip_position + distance)
        updated = replace(
            self,
            trip_position=next_pos
        )
        return updated

    def reroute(self, delay, current_time):
        updated = replace(
            self,
            delay=self.delay + delay,
            replannings=self.replannings + 1,
            most_recent_route_assignment_time=current_time
        )
        return updated

    def update_balance(self, value: int, max_balance: int) -> Tuple[Driver, int]:
        """
        increments (or decrements) the agent's balance, returning
        the updated driver and any overflow funds exceeding a driver's max
        """
        updated_balance = self.balance + value
        if updated_balance < 0:
            msg = (
                f"agent {self.driver_id} attempting to decrement "
                f"balance by {value} from {self.balance} to {updated_balance}. "
                f"driver: {self}"
            )
            raise ValueError(msg)
        elif updated_balance > max_balance:
            overflow = updated_balance - max_balance
            return replace(self, balance=max_balance), overflow
        else:
            return replace(self, balance=updated_balance), 0
