
import math

LIMIT = 1000001

all_sums = [0 for n in range(LIMIT)]
all_lengths = [0 for n in range(LIMIT)]

for i in range(1, LIMIT):
    n = 2 * i
    while n < LIMIT:
        all_sums[n] += i
        n += i

for i in range(1, LIMIT):
    n = i
    visited = set()
    while n not in visited:
        visited.add(n)
        n = all_sums[n]
        if n <= 0 or n > LIMIT - 1:
            all_lengths[i] = 0
            break
        if n < i:
            # We don't care because it's already captured by a smaller value
            all_lengths[i] = 0
            break
    if n == i:
        all_lengths[i] = len(visited)
    else:
        all_lengths[i] = 0

best = 0
best_idx = 0
for i in range(1, LIMIT):
    if all_lengths[i] > best:
        best = all_lengths[i]
        best_idx = i
print(best_idx)
