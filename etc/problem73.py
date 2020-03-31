
import math

total = 0

for d in range(4, 12001):
    n0 = math.floor(d / 3)
    n1 = math.ceil(d / 2)
    print(d, n0, n1)
    for n in range(n0 + 1, n1):
        if math.gcd(n, d) == 1:
            total += 1

print(total)
