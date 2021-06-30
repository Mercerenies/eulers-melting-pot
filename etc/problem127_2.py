
from functools import reduce

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

_factors = [None for _ in range(LIMIT)]

def factors(n):
    if _factors[n] is None:
        result = []
        n1 = n
        for i in primes:
            if n1 % i == 0:
                while n1 % i == 0:
                    n1 /= i
                result.append(i)
                if n1 == 1:
                    break
        _factors[n] = result
    return _factors[n]

def prod(lst):
    return reduce(lambda x, y: x * y, lst, 1)

by_radical = list(range(1, LIMIT))
by_radical.sort(key=lambda x: prod(factors(x)))

total_sum = 0
for c in range(1, LIMIT):
    if prod(factors(c)) < c:
        for a in by_radical:
            b = c - a
            if a >= b or b <= 0:
                continue
            if prod(factors(c)) * prod(factors(a)) * 2 > c:
                break
            fact_a = frozenset(factors(a))
            fact_b = frozenset(factors(b))
            fact_c = frozenset(factors(c))
            if fact_a & fact_b == frozenset():
                if prod(fact_a) * prod(fact_b) * prod(fact_c) < c:
                    total_sum += c
print(total_sum)
