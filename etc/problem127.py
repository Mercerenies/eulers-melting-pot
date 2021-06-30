
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

valid_c = set()
for c in range(1, LIMIT):
    if prod(factors(c)) < c:
        valid_c.add(c)

total_sum = 0
for a in range(1, LIMIT):
    fact_a = frozenset(factors(a))
    rad_a = prod(fact_a)
    print(a)
    for b in range(max(a + 1, rad_a - a + 1), LIMIT):
        c = a + b
        if c >= LIMIT:
            break
        if c in valid_c:
            fact_b = frozenset(factors(b))
            fact_c = frozenset(factors(c))
            if fact_a & fact_b == fact_b & fact_c == fact_a & fact_c == frozenset():
                if rad_a * prod(fact_b) * prod(fact_c) < c:
                    total_sum += c
print(total_sum)
