
"""Naive solution, just to get a feel for the pattern."""

from __future__ import annotations

from functools import cache, total_ordering
from dataclasses import dataclass, field, replace
from queue import PriorityQueue
from typing import Iterator


@cache
def is_prime(x: int) -> int:
    """Naive primality checker."""
    i = 2
    while i * i <= x:
        if x % i == 0:
            return False
        i += 1
    return True


def next_prime(n: int) -> int:
    """Next prime >= n."""
    while not is_prime(n):
        n += 1
    return n


@total_ordering
@dataclass(frozen=True)
class Sqube:
    p: int
    q: int
    value: int = field(init=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, 'value', self.p * self.p * self.q * self.q * self.q)  # noqa

    def __le__(self, other: Sqube) -> bool:
        return self.value <= other.value

    def is_proper(self) -> bool:
        return self.p != self.q


def all_squbes() -> Iterator[Sqube]:
    visited = set()
    frontier: PriorityQueue[Sqube] = PriorityQueue()
    frontier.put(Sqube(2, 2))
    while True:
        next_value = frontier.get()
        if next_value in visited:
            continue
        visited.add(next_value)
        if next_value.is_proper():
            yield next_value
        p1 = next_prime(next_value.p + 1)
        q1 = next_prime(next_value.q + 1)
        frontier.put(replace(next_value, p=p1))
        frontier.put(replace(next_value, q=q1))


def is_prime_proof(n: int) -> bool:
    """True if there are NO primes obtained by changing exactly one
    digit of n. Precondition: n is itself non-prime.

    """
    digits = list(str(n))
    if digits[-1] in '024568':
        # Easy case: We only need to check the final digit.
        for d in '0123456789':
            digits[-1] = d
            if is_prime(int(''.join(digits))):
                return False
        return True
    else:
        # Hard case: Check all digits.
        for i in range(len(digits)):
            original_value = digits[i]
            for d in '0123456789':
                digits[i] = d
                if is_prime(int(''.join(digits))):
                    return False
            digits[i] = original_value
        return True


if __name__ == "__main__":
    for i in range(1000):
        if is_prime_proof(i):
            print(i)

    '''
    desired_sqube_count = 10
    substr = '200'
    for sqube in all_squbes():
        if is_prime_proof(sqube.value) and substr in str(sqube.value):
            desired_sqube_count -= 1
            if not desired_sqube_count:
                break
    print(sqube.value)
    '''
