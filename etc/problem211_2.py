
# Too sloooow :(

from dataclasses import dataclass


@dataclass(frozen=True)
class CoprimePair:
    a: int
    b: int


@dataclass(frozen=True)
class PrimePower:
    p: int
    power: int

PERFECT_SQUARES = []
PERFECT_SQUARES_SET = set()

LIMIT = 64_000


def is_perfect_square(n: int) -> bool:
    while len(PERFECT_SQUARES) * len(PERFECT_SQUARES) <= n:
        x = len(PERFECT_SQUARES) * len(PERFECT_SQUARES)
        PERFECT_SQUARES.append(x)
        PERFECT_SQUARES_SET.add(x)
    return n in PERFECT_SQUARES_SET


def find_coprime_pair(n: int) -> CoprimePair | PrimePower:
    for i in range(2, n + 1):
        # i is a prime number dividing into n, so some power of i,
        # together with n divided by that number, forms a coprime pair
        # whose product is n.
        if n % i == 0:
            j = 0
            b = 1
            while n % i == 0:
                b *= i
                n //= i
                j += 1
            if n == 1:
                # n is a prime power.
                return PrimePower(p=i, power=j)
            return CoprimePair(n, b)
    assert False


SIGMA2 = [0, 1]
for i in range(2, LIMIT):
    # Calculate sigma2
    result = find_coprime_pair(i)
    if isinstance(result, CoprimePair):
        SIGMA2.append(SIGMA2[result.a] * SIGMA2[result.b])
    else:
        SIGMA2.append(sum(result.p ** (2 * j) for j in range(0, result.power + 1)))

for i, sigma in enumerate(SIGMA2):
    if i >= 1 and is_perfect_square(sigma):
        print(i)
