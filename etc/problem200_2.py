
"""Adding custom priority queue implementation.

"""

from __future__ import annotations

from functools import total_ordering
from dataclasses import dataclass, field, replace
from typing import Iterator, Protocol


class Ordered(Protocol):
    def __le__[CT](self: CT, other: CT) -> bool:
        ...

    def __lt__[CT](self: CT, other: CT) -> bool:
        ...


def _pq_parent(n: int) -> int:
    return (n + 1) // 2 - 1


def _pq_left_child(n: int) -> int:
    return (n + 1) * 2 - 1


def _pq_right_child(n: int) -> int:
    return (n + 1) * 2


class PriorityQueue[T: Ordered]:
    # Complete binary tree stored in pre-order.
    _tree: list[T]

    def __init__(self) -> None:
        self._tree = []

    def put(self, value: T) -> None:
        curr_index = len(self._tree)
        self._tree.append(value)
        while curr_index != 0:
            parent_index = _pq_parent(curr_index)
            if self._tree[parent_index] <= self._tree[curr_index]:
                break
            curr_index = parent_index

    def pop(self) -> T:
        if not self._tree:
            raise IndexError
        return_value = self._tree[0]
        if len(self._tree) == 1:
            # Tree is a singleton, so empty the list.
            self._tree.clear()
            return return_value
        self._tree[0] = self._tree.pop()
        curr_index = 0
        while curr_index < len(self._tree):
            left_index = _pq_left_child(curr_index)
            right_index = _pq_right_child(curr_index)
            smaller_index = -1
            if left_index < len(self._tree) and self._tree[left_index] < self._tree[curr_index]:
                smaller_index = left_index
            if right_index < len(self._tree) and self._tree[right_index] < self._tree[curr_index]:
                if smaller_index == -1 or self._tree[right_index] < self._tree[smaller_index]:
                    smaller_index = right_index
            if smaller_index == -1:
                break  # Done swapping.
            self._tree[smaller_index], self._tree[curr_index] = self._tree[curr_index], self._tree[smaller_index]
            curr_index = smaller_index
        return return_value


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
        next_value = frontier.pop()
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
            print(sqube.value)
            desired_sqube_count -= 1
            if not desired_sqube_count:
                break
    print(sqube.value)

# So, notes on problem200.fal: Miller Rabin overflows 64-bit integers
# but naive primality works. I can't believe I wrote like six Miller
# Rabin checks for this when I didn't need any of them.
#
# Old Falcon code for Miller Rabin is at problem200_millerrabin.fal
