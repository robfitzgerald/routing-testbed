from __future__ import annotations
from enum import Enum
from dataclasses import dataclass, replace
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
    trip_start_time: int
    original_trip_total: int
    trip_position: int = 0
    delay: int = 0
    replannings: int = 0
    most_recent_replanning_time: Optional[int] = None
    state: DriverState = DriverState.INACTIVE

    def trip_total(self) -> int:
        return self.original_trip_total + self.delay

    def trip_pct(self) -> float:
        return float(self.trip_position) / float(self.trip_total())

    def delay_pct(self) -> float:
        return float(self.delay) / float(self.trip_total())

    def pct_original_of_final(self) -> float:
        return float(self.original_trip_total) / float(self.trip_total())

    def observation(self):
        pct_finished = self.trip_pct()
        pct_delay = self.delay_pct()
        return [self.balance, pct_finished, pct_delay]

    def move(self, distance):
        next_pos = min(self.trip_total(), self.trip_position + distance)
        next_state = DriverState.ARRIVED if next_pos == self.trip_total() else self.state
        updated = replace(
            self,
            state=next_state,
            trip_position=next_pos
        )
        return updated

    def start_trip(self, current_time) -> Driver:
        updated = replace(
            self,
            state=DriverState.ACTIVE,
            most_recent_replanning_time=current_time
        )
        return updated

    def reroute(self, delay, current_time):
        updated = replace(
            self,
            delay=self.delay + delay,
            replannings=self.replannings + 1,
            most_recent_replanning_time=current_time
        )
        return updated

    def increment_balance(self, value: int, max_balance: int) -> Tuple[Driver, int]:
        """
        increments (decrements also) the agent's balance, returning
        the updated driver and any overflow funds exceeding a driver's max
        """
        updated_balance = self.balance + value
        if updated_balance < 0:
            msg = (
                f"agent {self.driver_id} attempting to decrement "
                f"balance to {updated_balance} (increment of {value}). "
                f"driver: {self}"
            )
            raise Exception(msg)
        elif updated_balance > max_balance:
            overflow = updated_balance - max_balance
            return replace(self, balance=max_balance), overflow
        else:
            return replace(self, balance=updated_balance), 0

    def report_done(self) -> Driver:
        return replace(self, state=DriverState.DONE)

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
