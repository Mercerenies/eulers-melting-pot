
# Attempt to brute-force 3 as well as the other odd primes. Too slow,
# use problem152_3.py instead.

from __future__ import annotations

from itertools import *
from fractions import Fraction
from typing import Iterable, Iterator, Generic, TypeVar, cast

UPPER_LIMIT = 80

T = TypeVar("T")

def powerset(iterable: Iterable[T]) -> Iterable[tuple[T, ...]]:
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


def is_prime(x: int) -> bool:
    i = 2
    while i * i <= x:
        if x % i == 0:
            return False
        i += 1
    return True


class SetInUniverse(Generic[T]):

    def __init__(self, inner_set: set[T], outer_set: set[T]):
        assert inner_set <= outer_set
        self.inner_set = inner_set
        self.outer_set = outer_set

    def __repr__(self) -> str:
        return f"SetInUniverse({self.inner_set},{self.outer_set})"

    def __str__(self) -> str:
        return str(self.inner_set)

    def __iter__(self) -> Iterator[T]:
        return iter(self.inner_set)

    def join_in_context(self, other: SetInUniverse[T]) -> SetInUniverse[T] | None:
        # Joins the two sets. The resulting set will be in the union
        # of the two universes. The sets must agree on the
        # intersection of the two universes, or this function will
        # return None.
        universe = self.outer_set | other.outer_set
        intersection = self.outer_set & other.outer_set
        for x in intersection:
            if (x in self.inner_set) != (x in other.inner_set):
                # The two disagree; bail out.
                return None
        return SetInUniverse(self.inner_set | other.inner_set, universe)


def p_adic_valuation(x: int, p: int) -> int:
    result = 0
    while x % p == 0:
        x //= p
        result += 1
    return result


# Precondition: p is an odd prime.
def analyze_prime(p: int, desired_valuation: int) -> Iterable[SetInUniverse]:
    multiples = range(p, UPPER_LIMIT + 1, p)
    multiples_set = set(multiples)
    for candidate in powerset(multiples):
        current_sum = sum(Fraction(1, x * x) for x in candidate)
        if p_adic_valuation(current_sum.denominator, p) == desired_valuation:
            yield SetInUniverse(set(candidate), multiples_set)


def merge(xs: Iterable[SetInUniverse], ys: Iterable[SetInUniverse]) -> Iterable[SetInUniverse]:
    for x, y in product(xs, ys):
        joined = x.join_in_context(y)
        if joined is not None:
            yield joined

brute_force_values = list(range(2, UPPER_LIMIT + 1))

all_possible_tuples: list[SetInUniverse[int]]
all_possible_tuples = [SetInUniverse(set(), set())]
for i in range(3, UPPER_LIMIT + 1):
    if is_prime(i):
        new_tuples = analyze_prime(i, desired_valuation=0)
        all_possible_tuples = list(merge(all_possible_tuples, new_tuples))
        # Remove multiples of i from consideration.
        brute_force_values = [x for x in brute_force_values if x % i != 0]

target_sums: dict[Fraction, int]
target_sums = {}
for tup in all_possible_tuples:
    current_sum = sum(Fraction(1, x * x) for x in tup)
    new_target = Fraction(1, 2) - current_sum
    target_sums[new_target] = target_sums.get(new_target, 0) + 1

solution_count = 0
for candidate in powerset(brute_force_values):
    if not candidate:
        continue
    s = sum(Fraction(1, x * x) for x in candidate)
    solution_count += target_sums.get(cast(Fraction, s), 0)
print(solution_count)
