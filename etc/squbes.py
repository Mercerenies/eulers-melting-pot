
"""Playing around with squbes from Problem 200."""

from __future__ import annotations

from functools import cache


@cache
def is_prime(x: int) -> int:
    """Naive primality checker."""
    i = 2
    while i * i <= x:
        if x % i == 0:
            return False
        i += 1
    return True


def sqube(p: int, q: int) -> int:
    return p * p * q * q * q


def is_prime_proof(n: int) -> bool:
    """True if there are NO primes obtained by changing at most one
    digit of n.

    """
    if is_prime(n):
        return False

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
    for i in range(10000000):
        if i % 10 in (1, 3, 7, 9) and is_prime_proof(i):
            print(i)
