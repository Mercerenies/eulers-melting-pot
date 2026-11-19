
# Got it down to 1m 52s using the multiplicative properties of sigma2

import math


LIMIT = 64_000_000

SMALLEST_DIVISOR = [-1 for i in range(LIMIT)]
SIGMA2 = [-1 for i in range(LIMIT)]

for n in range(2, LIMIT):
    if SMALLEST_DIVISOR[n] == -1:
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
        if SMALLEST_DIVISOR[j] == -1:
            SMALLEST_DIVISOR[j] = n
        j += n

print("A")

# Now calculate SIGMA2 for numbers that are not prime powers.
total = 1  # We skip over the trivial case of n = 1.
for n in range(2, LIMIT):
    if SIGMA2[n] == -1:
        d = SMALLEST_DIVISOR[n]
        a = d
        while n % (a * d) == 0:
            a *= d
        b = n // a
        SIGMA2[n] = SIGMA2[a] * SIGMA2[b]
    s = SIGMA2[n]
    q = round(math.sqrt(s))
    if q * q == s:
        total += n

print(SIGMA2[:100])

print(total)
