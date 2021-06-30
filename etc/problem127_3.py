
from functools import reduce
import math

LIMIT = 120000

sieve = [True for _ in range(LIMIT)]
sieve[0] = False
sieve[1] = False
for i in range(LIMIT):
    if sieve[i]:
        j = i + i
        while j < LIMIT:
            sieve[j] = False
            j += i

primes = []
for i in range(LIMIT):
    if sieve[i]:
        primes.append(i)

rads = [1 for i in range(1, LIMIT)]

def _rad(n):
    result = 1
    n1 = n
    for i in primes:
        if n1 % i == 0:
            while n1 % i == 0:
                n1 /= i
            result *= i
            if n1 == 1:
                break
    return result

rads = [1 for i in range(LIMIT)]
for i in primes:
    j = i
    while j < LIMIT:
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
