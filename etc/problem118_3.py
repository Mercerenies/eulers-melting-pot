
# Okay, this time we're doing it imperative and preventing all of the
# recursion we possibly can >.<

import itertools

MAXIMUM = 9
DIGITS = frozenset(range(1, MAXIMUM+1))

hits = {}
hits1 = {}

def _is_prime(x):
    if x < 2:
        return False
    for i in range(2, x // 2 + 1):
        if x % i == 0:
            return False
    return True

def is_prime(x):
    if x not in hits:
        hits[x] = _is_prime(x)
    return hits[x]

def to_number(lst):
    acc = 0
    for x in lst:
        acc = 10 * acc + x
    return acc

def _count_solutions(lst):
    if sum(lst) % 3 == 0:
        return 0 # All permutations will be divisible by 3
    count = 0
    for xs in itertools.permutations(lst):
        if is_prime(to_number(xs)):
            count += 1
    return count

def count_solutions(lst):
    tmp = frozenset(lst)
    if tmp not in hits1:
        hits1[tmp] = _count_solutions(lst)
    return hits1[tmp]

def _all_subsets(lst, idx):
    if idx >= len(lst):
        return [[]]
    else:
        rest = _all_subsets(lst, idx + 1)
        return rest + list(map(lambda xs: [lst[idx]] + xs, rest))

def all_subsets(lst):
    return _all_subsets(lst, 0)

def recurse(used):

    # Find the smallest unused value
    smallest_unused = 0
    for x in range(1, MAXIMUM+1):
        if x not in used:
            smallest_unused = x
            break
    if smallest_unused == 0:
        print("Out")
        return 1

    count = 0

    remaining = list(DIGITS - {smallest_unused} | used)
    for sub in all_subsets(remaining):
        sub.append(smallest_unused)
        print("outer", sub)
        current_count = count_solutions(sub)
        print("counted")
        if current_count != 0:
            print("in", sub)
            rest_count = recurse(used | frozenset(sub))
            count += current_count * rest_count

    return count

def run():
    return recurse(frozenset())

print(run())
