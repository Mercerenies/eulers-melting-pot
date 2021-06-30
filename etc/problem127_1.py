
from functools import reduce
import sys

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

def is_valid(a, b):
    if a >= b:
        return False
    if a + b >= LIMIT:
        return False
    c = a + b
    fact_a = frozenset(factors(a))
    fact_b = frozenset(factors(b))
    fact_c = frozenset(factors(c))
    if fact_a & fact_b == fact_b & fact_c == fact_a & fact_c == frozenset():
        if prod(fact_a) * prod(fact_b) * prod(fact_c) < c:
            return True
    return False

for i in range(1, LIMIT):
    factors(i) # Go ahead and generate all of them

total = 0

def recurse(a, b, prime_idx):
    global total
    if prime_idx >= len(primes):
        if is_valid(a, b):
            total += a + b
        return
    if a + b >= LIMIT:
        return
    # Try skipping the prime entirely
    recurse(a, b, prime_idx + 1)
    # Try putting the value into a
    a1 = a
    while a1 < LIMIT:
        a1 *= primes[prime_idx]
        recurse(a1, b, prime_idx + 1)
    # Try putting the value into b
    b1 = b
    while b1 < LIMIT:
        b1 *= primes[prime_idx]
        recurse(a, b1, prime_idx + 1)

sys.setrecursionlimit(2 * len(primes)) # I'm going to regret this

recurse(1, 1, 0)
print(total)
