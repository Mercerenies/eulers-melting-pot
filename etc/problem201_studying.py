
# Studying some patterns in problem201.
#
# Note: problem201_3.cs is working in about 12s, this is
# extracurricular.

from functools import cache
from dataclasses import dataclass


@dataclass(init=False, frozen=True)
class Sum:
    terms: tuple[int, ...]

    def __init__(self, *terms: int) -> None:
        object.__setattr__(self, 'terms', terms)


def sum_squares(a: int, b: int | None = None) -> int:
    if b is None:
        return a * (a + 1) * (2 * a + 1) // 6
    else:
        return sum_squares(b) - sum_squares(a - 1)


@cache
def enumerate_sums(target: int, max_index: int, terms: int) -> tuple[Sum, ...]:
    if terms == 0:
        if target == 0:
            return (Sum(),)  # Empty sum
        else:
            return ()
    elif terms < 0 or max_index < terms or target <= 0:
        return ()  # No solution
    elif target < sum_squares(terms) or target > sum_squares(max_index - terms + 1, max_index):
        # Short-circuit because we're out of bounds
        return ()
    else:
        solutions = []
        for i in range(max_index, 0, -1):
            new_target = target - i * i
            for solution in enumerate_sums(new_target, i - 1, terms - 1):
                solution = Sum(*solution.terms, i)
                solutions.append(solution)
        return tuple(solutions)


if __name__ == "__main__":
    sums = list(enumerate_sums(45_048, 100, 50))
    s0, s1, s2 = sums
    print("s0 =", s0)
    print("s1 =", s1)
    print("s2 =", s2)
    print("Difference between s0 and s1: " + str(set(s1.terms) - set(s0.terms)) + " and " + str(set(s0.terms) - set(s1.terms)))
    print("Difference between s0 and s2: " + str(set(s2.terms) - set(s0.terms)) + " and " + str(set(s0.terms) - set(s2.terms)))
    print("Difference between s1 and s2: " + str(set(s2.terms) - set(s1.terms)) + " and " + str(set(s1.terms) - set(s2.terms)))
