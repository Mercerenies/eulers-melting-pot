
# Just testing some properties of special sum sets (SSS).
#
# Obvious things:
#
# 1. If we have an SSS, we can get another SSS by multiplying by a
# positive integer.
#
# 2. If we have an SSS whose gcd is > 1, then we can divide by the gcd
# to get a strictly better SSS, so it's only worth considering sets
# whose gcd is 1.

from collections import namedtuple

Subset = namedtuple("Subset", ['size', 'sum', 'hash'])

def _generate_subsets(s, n):
    if n >= len(s):
        return [Subset(0, 0, hash(()))]
    rest = _generate_subsets(s, n + 1)
    result = rest[:]
    for curr in rest:
        result.append(Subset(curr.size + 1, curr.sum + s[n], hash((n, curr))))
    return result

def generate_subsets(s):
    return _generate_subsets(s, 0)

# Exponential time, do not use for brute forcing!
def verify_for_set(s):
    for sub1 in generate_subsets(s):
        for sub2 in generate_subsets(s):
            if sub1.size > sub2.size and sub1.sum <= sub2.sum:
                return False
            if sub1.sum == sub2.sum and sub1.hash != sub2.hash:
                return False
    return True

def combine(a, b, n):
    return a + list(map(lambda x: x + n, b))

print(verify_for_set([1]))
print(verify_for_set([1, 2]))
print(verify_for_set([2, 3, 4]))
print(verify_for_set([3, 5, 6, 7]))
print(verify_for_set([6, 9, 11, 12, 13]))
print(verify_for_set([11, 17, 20, 22, 23, 24]))
print(verify_for_set([11, 18, 19, 20, 22, 25]))

#for i in range(500):
#    if verify_for_set(combine([11], [6, 9, 11, 12, 13], i)):
#        print(i)
#        break

#print(verify_for_set([22, 33, 39, 42, 44, 45, 46]))
