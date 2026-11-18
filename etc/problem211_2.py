
# Too sloooow :(
#
# Still wrong, possible memory overflow somehow...

from dataclasses import dataclass


PERFECT_SQUARES = []
PERFECT_SQUARES_SET = set()

def is_perfect_square(n: int) -> bool:
    while len(PERFECT_SQUARES) * len(PERFECT_SQUARES) <= n:
        x = len(PERFECT_SQUARES) * len(PERFECT_SQUARES)
        PERFECT_SQUARES.append(x)
        PERFECT_SQUARES_SET.add(x)
    return n in PERFECT_SQUARES_SET


LIMIT = 64_000_000

LARGEST_COPRIME_DIVISOR = [-1 for i in range(LIMIT)]
SIGMA2 = [-1 for i in range(LIMIT)]

for n in range(2, LIMIT):
    if LARGEST_COPRIME_DIVISOR[n] == -1:
        # We've found a prime
        j = n
        k = 1 + n * n
        while j < LIMIT:
            SIGMA2[j] = k
            j *= n
            k += j * j
    # Mark coprime divisors
    j = n * 2
    while j < LIMIT:
        LARGEST_COPRIME_DIVISOR[j] = n
        j += n

print("A")

# Now calculate SIGMA2 for numbers that are not prime powers.
for n in range(2, LIMIT):
    if SIGMA2[n] != -1:
        continue
    a = LARGEST_COPRIME_DIVISOR[n]
    b = n // a
    SIGMA2[n] = SIGMA2[a] * SIGMA2[b]

print("B")

total = 0
for i, sigma in enumerate(SIGMA2[1:], 1):
    if i % 10000 == 0:
        print(i)
    if is_perfect_square(sigma):
        total += i
print(total)
