
from math import comb
from functools import reduce
import operator

def pascal(n):
    # Nth row of Pascal's triangle
    acc = []
    for r in range(0, n + 1):
        acc.append(comb(n, r))
    return acc

def count(n):
    return sum(1 for k in pascal(n) if k % 7 != 0)

def in_base_7(n):
    """Returns list of digits from lsb to msb"""
    result = []
    while n != 0:
        result.append(n % 7)
        n //= 7
    return result

def count_using_base_7(n):
    return reduce(operator.mul, (k + 1 for k in in_base_7(n)), 1)

# Do the base 7 thing w/o the intermediate list.
def fast_count(n):
    total = 1
    while n != 0:
        total *= ((n % 7) + 1)
        n //= 7
    return total

"""
print(pascal(56))
print([(x % 7 == 0) for x in pascal(56)])
exit(0)
"""

k = 0
for i in range(0, 1000000000):
    if i % 1000000 == 0:
        print(i)
    #print(i, count_using_base_7(i))
    k += fast_count(i)
print(k)
