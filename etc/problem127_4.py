
from functools import reduce
import math

LIMIT = 120000

sieve = [True for _ in range(LIMIT)]
rads = [1 for i in range(LIMIT)]
sieve[0] = False
sieve[1] = False

for i in range(LIMIT):
    if sieve[i]:
        rads[i] *= i
        j = i + i
        while j < LIMIT:
            sieve[j] = False
            rads[j] *= i
            j += i

by_radical = list(range(1, LIMIT))
by_radical.sort(key=lambda x: rads[x])

total_sum = 0
for c in range(1, LIMIT):
    if rads[c] < c:
        for a in by_radical:
            b = c - a
            if a >= b or b <= 0:
                continue
            if rads[c] * rads[a] * 2 > c:
                break
            if math.gcd(a, b) == 1:
                if rads[a] * rads[b] * rads[c] < c:
                    total_sum += c
print(total_sum)
