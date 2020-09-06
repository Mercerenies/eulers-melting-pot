
import math

LIMIT = 1000001

def sum_of_div(n):
    total = 0
    for i in range(2, math.ceil(math.sqrt(n))):
        if n % i == 0:
            total += i
            if i * i != n:
                total += int(n / i) # Don't count sqrt twice
    return total + 1

all_sums = [sum_of_div(n) for n in range(LIMIT)]
all_lengths = [0 for n in range(LIMIT)]

print("ok")

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

print("ok")

best = 0
best_idx = 0
for i in range(1, LIMIT):
    if all_lengths[i] > best:
        best = all_lengths[i]
        best_idx = i
print(best_idx)
