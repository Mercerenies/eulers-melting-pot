
# Trying to do clever shenanigans with sigma2 but it's way slower.
# Probably correct but I didn't wait for it to terminate. Prefer
# problem211.py.


PERFECT_SQUARES = []
PERFECT_SQUARES_SET = set()


def is_perfect_square(n: int) -> bool:
    while len(PERFECT_SQUARES) * len(PERFECT_SQUARES) <= n:
        x = len(PERFECT_SQUARES) * len(PERFECT_SQUARES)
        PERFECT_SQUARES.append(x)
        PERFECT_SQUARES_SET.add(x)
    return n in PERFECT_SQUARES_SET


def sigma2(n: int) -> int:
    total = 1
    k = 2
    while k <= n:
        if n % k == 0:
            this_sum = 1
            x = k
            while n % k == 0:
                this_sum += x * x
                x *= k
                n //= k
            total *= this_sum
        k += 1
    return total


LIMIT = 64_000_000


sums_of_squares = [1 for _ in range(LIMIT)]

biggest = 0

total = 0
for i in range(1, LIMIT):
    if i % 10_000 == 0:
        print(i)
    if is_perfect_square(sigma2(i)):
        print(i)
        total += i


print(total)
