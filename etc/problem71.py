
import math

# 2/5 is a known fraction less than 3/7
ln = 2
ld = 5

for d in range(1, 1000001):
    for n in range((ln * d) // ld, d // 2 + 1):
        if 7 * n >= 3 * d:
            break
        if ln * d < n * ld:
            ln = n
            ld = d

print(ln // math.gcd(ln, ld))
