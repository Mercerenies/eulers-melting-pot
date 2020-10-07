
# Note: "Algorithmic" solution for n=7 is
#
# {20, 31, 38, 39, 40, 42, 45} (sum = 255)

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
# Require that 6 a + 21 < 255
for a in range(1, 39):
    # Require that a + 5 b + 15 < 255
    for b in range(a + 1, (240 - a) // 5 + 1):
        # Require that a + b + 4 c + 10 < 255
        for c in range(b + 1, min(a + b, (245 - a - b) // 4 + 1)):
            # With only three elements, we know our conditions pretty
            # well. The only way to break Condition 1 is if a + b ==
            # c, and the only way to break Condition 2 is if a + b <
            # c, so we require simply that a + b > c in our loop
            # condition.
            trios.append((a, b, c))

quads = []
# 4 a + 6 < 255
for a in range(5, 630):
    # a + 3 b + 3 < 255
    for b in range((252 - a) // 3 + 1):
        # a + b + 2 c + 2 < 255
        for c in range(b + 1, (253 - a - b) // c + 1):
            for d in range(c + 1, 255 - a - b - c):
                arr = (a, b, c, d)
                if sum(arr) > 246:
                    # No room for the other three
                    continue
                if verify(arr):
                    quads.append(arr)

trios = list(reversed(trios))
quads = list(reversed(quads))

best = 255
best_value = ()

print(len(trios))
i = 0
for x in trios:
    for y in quads:
        if x[-1] >= y[0]:
            break
        arr = x + y
        if reduce(math.gcd, arr) != 1:
            continue
        if sum(arr) > best:
            continue
        if sum(arr) < 128:
            # Exhaustiveness argument says this can't work
            continue
        if verify(arr):
            if sum(arr) < best:
                best_value = arr
                best = sum(arr)
    i += 1
    if i % 10 == 0:
        print(i)

print(''.join(best_value))

