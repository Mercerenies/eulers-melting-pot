
import itertools
from functools import reduce

MAXIMUM = 9
DIGITS = frozenset(range(1, MAXIMUM+1))

PRIME_UPPER = 99999999
sieve = [True for _ in range(PRIME_UPPER)]

hits1 = {}

def _is_prime(x):
    if x < 2:
        return False
    for i in range(2, x // 2 + 1):
        if x % i == 0:
            return False
    return True

def is_prime(x):
    if x < PRIME_UPPER:
        return sieve[x]
    return _is_prime(x)

def to_number(lst):
    acc = 0
    for x in lst:
        acc = 10 * acc + x
    return acc

def count_solutions(lst):
    if len(lst) > 1 and sum(lst) % 3 == 0:
        return 0 # All permutations will be divisible by 3
    count = 0
    for xs in itertools.permutations(lst):
        if is_prime(to_number(xs)):
            count += 1
    return count

def _all_subsets(lst, idx):
    if idx >= len(lst):
        return [[]]
    else:
        rest = _all_subsets(lst, idx + 1)
        return rest + list(map(lambda xs: [lst[idx]] + xs, rest))

def all_subsets(lst):
    return _all_subsets(lst, 0)

def all_parts(lst):
    if not lst:
        yield []
    else:
        for sub in all_subsets(lst[1:]):
            sub.append(lst[0])
            remaining = sorted(frozenset(lst) - frozenset(sub))
            for xs in all_parts(remaining):
                yield [sub] + xs

# Sieve
sieve[0] = False
sieve[1] = False
for i in range(PRIME_UPPER):
    if sieve[i]:
        j = i + i
        while j < PRIME_UPPER:
            sieve[j] = False
            j += i

count = 0
for part in all_parts(list(DIGITS)):
    inner = 1
    for xs in part:
        tmp = count_solutions(xs)
        if tmp == 0:
            inner = 0
            break
        inner *= tmp
    count += inner
print(count)
