
"""Adding Miller Rabin prime test.

6 seconds in Python.

Down to 4 seconds with better iteration order in squbes generation.

"""

from __future__ import annotations

from functools import total_ordering
from dataclasses import dataclass, field, replace
from queue import PriorityQueue
from typing import Iterator


def is_prime_naive(x: int) -> bool:
    """Naive primality checker."""
    i = 2
    while i * i <= x:
        if x % i == 0:
            return False
        i += 1
    return True


def pow_mod(a, b, n):
    if b == 0:
        return 1
    elif b % 2 == 0:
        x = pow_mod(a, b // 2, n)
        return (x * x) % n
    else:
        return (pow_mod(a, b - 1, n) * a) % n


def miller_rabin_test(n, d, r, a):
    x = pow_mod(a, d, n)
    if x == 1 or x == n - 1:
        return True
    for _i in range(r):
        x = (x * x) % n
        if x == n - 1:
            return True
    return False


def is_prime(n: int) -> bool:
    if n < 2:
        return False
    if n == 2 or n == 3 or n == 5 or n == 13 or n == 23 or n == 1662803:
        return True
    if n % 2 == 0 or n % 3 == 0 or n % 5 == 0:
        return False
    if n >= 1_122_004_669_633:
        return is_prime_naive(n)

    d = n - 1
    r = 0
    while d % 2 == 0:
        d //= 2
        r += 1

    if not miller_rabin_test(n, d, r, 2):
        return False
    if not miller_rabin_test(n, d, r, 13):
        return False
    if not miller_rabin_test(n, d, r, 23):
        return False
    if not miller_rabin_test(n, d, r, 1662803):
        return False
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
    frontier: PriorityQueue[Sqube] = PriorityQueue()
    frontier.put(Sqube(2, 2))
    while True:
        next_value = frontier.get()
        if next_value.is_proper():
            yield next_value
        p1 = next_prime(next_value.p + 1)
        frontier.put(replace(next_value, p=p1))
        if next_value.p == 2:
            q1 = next_prime(next_value.q + 1)
            frontier.put(replace(next_value, q=q1))


def is_prime_proof(n: int) -> bool:
    """True if there are NO primes obtained by changing exactly one
    digit of n. Precondition: n is itself non-prime.

    """

    # Amusingly, it seems like all of the squbes containing 200 within
    # our domain space are actually in the "easy case". But that's not
    # true of squbes in general, so I can't exploit that fact here.
    if n % 10 in (0, 2, 4, 5, 6, 8):
        # Easy case: We only need to check the final digit.
        n = (n // 10) * 10
        for k in (1, 3, 7, 9):
            if is_prime(n + k):
                return False
        return True
    else:
        # Hard case: Check all digits.
        digits = list(str(n))
        for i in range(len(digits)):
            original_value = digits[i]
            for d in '0123456789':
                if d == '0' and i == 0:
                    continue  # Can't replace leading digit with zero.
                digits[i] = d
                if is_prime(int(''.join(digits))):
                    return False
            digits[i] = original_value
        return True


if __name__ == "__main__":
    desired_sqube_count = 200
    substr = '200'
    for sqube in all_squbes():
        if substr in str(sqube.value) and is_prime_proof(sqube.value):
            desired_sqube_count -= 1
            print(desired_sqube_count, sqube.value)
            if not desired_sqube_count:
                break
    print(sqube.value)
