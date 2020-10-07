
# Note: "Algorithmic" solution for n=7 is
#
# {20, 31, 38, 39, 40, 42, 45} (sum = 255)
#
# Works for n=6 (takes like 10 minutes)

from functools import reduce
import itertools
import math

def verify(arr):
    # Condition 1
    sums = {0}
    for u in arr:
        sums1 = set()
        for s in sums:
            if s + u in sums:
                return False # Duplicate sum
            sums1.add(s + u)
        sums |= sums1
    # Condition 2
    if arr[0] + arr[1] < arr[-1]:
        return False
    if arr[0] + arr[1] + arr[2] < arr[-1] + arr[-2]:
        return False
    if arr[0] + arr[1] + arr[2] + arr[3] < arr[-1] + arr[-2] + arr[-3]:
        return False
    return True

trios = []
# Require that 3 a + 3 < 255
for a in range(1, 85):
    # Require that a + 2 b + 1 < 255
    for b in range(a + 1, (254 - a) // 2 + 1):
        # Require that a + b + c < 255
        for c in range(b + 1, min(a + b, 255 - a - b)):
            # With only three elements, we know our conditions pretty
            # well. The only way to break Condition 1 is if a + b ==
            # c, and the only way to break Condition 2 is if a + b <
            # c, so we require simply that a + b > c in our loop
            # condition.
            trios.append((a, b, c))

trios = list(reversed(trios))

best = 255
best_value = ()

sextuples = []
i = 0
for x in trios:
    for y in trios:
        if x[2] >= y[0]:
            break
        arr = x + y
        if reduce(math.gcd, arr) != 1:
            continue
        if sum(arr) > best:
            continue
        if verify(arr):
            sextuples.append(arr)
            if sum(arr) < best:
                best_value = arr
                best = sum(arr)
    i += 1
    if i % 1000 == 0:
        print(i)

print(best_value, best)

