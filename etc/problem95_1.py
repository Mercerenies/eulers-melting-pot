
import math

LIMIT = 60000

sigma_cache = []

def pentagonal(k):
    return (k * (3 * k - 1)) // 2

def sigma_cached(m, n):
    if n == 0:
        return m
    elif n < 0:
        return 0
    else:
        return sigma_cache[n]

def sum_of_div(n):
    total = 0
    for i in range(2, math.ceil(math.sqrt(n))):
        if n % i == 0:
            total += i
            if i * i != n:
                total += int(n / i) # Don't count sqrt twice
    return total + 1

sigma_cache.append(0)
sigma_cache.append(1)
for m in range(2, LIMIT):
    total = 0
    k = 1
    while pentagonal(k) <= m:
        sgn = -1 if k % 2 == 0 else 1
        total += sgn * (sigma_cached(m, m - pentagonal(k)) + sigma_cached(m, m - pentagonal(-k)))
        k += 1
    sigma_cache.append(total)

print(sigma_cache[-10:])
exit(1)

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

print(sigma_cache[:10])

best = 0
best_idx = 0
for i in range(1, LIMIT):
    if all_lengths[i] > best:
        best = all_lengths[i]
        best_idx = i
print(best_idx)
