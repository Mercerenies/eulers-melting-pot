
# Note: "Algorithmic" solution for n=7 is
#
# {20, 31, 38, 39, 40, 42, 45} (sum = 255)
#
# 8 sec, finally...

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

best = 255
best_value = ()

# Require that 7 a + 21 <= 255
for a in range(1, 35):
    # Require that a + 6 b + 15 <= 255
    for b in range(a + 1, (241 - a) // 6 + 1):
        # Require that a + b + 5 c + 10 <= 255
        for c in range(b + 1, min(a + b, (246 - a - b) // 5 + 1)):
            # Require that a + b + c + 4 d + 6 <= 255
            for d in range(c + 1, min(a + b, (250 - a - b - c) // 4 + 1)):
                # Require that a + b + c + d + 3 e + 3 <= 255
                for e in range(d + 1, min(a + b, (251 - a - b - c - d) // 3 + 1)):
                    # Require that a + b + c + d + e + 2 f + 1 <= 255
                    for f in range(e + 1, min(a + b, (255 - a - b - c - d - e) // 2 + 1)):
                        # Require that a + b + c + d + e + f + g <= 255
                        for g in range(f + 1, min(a + b, 256 - a - b - c - d - e - f)):
                            arr = (a, b, c, d, e, f, g)
                            if reduce(math.gcd, arr) != 1:
                                continue
                            if sum(arr) > best or sum(arr) < 128:
                                continue
                            if verify(arr):
                                best_value = arr
                                best = sum(arr)

print(''.join(map(str, best_value)))
