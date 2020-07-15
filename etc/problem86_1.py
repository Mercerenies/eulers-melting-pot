
import math

# b <= c <= a

total = 0

def enumerate_for_value(x):
    global total
    m = 1
    n = 1
    while True:
        if n >= m:
            n = 1
            m += 1
            if m > x:
                break
            continue
        a = x
        # If a = x
        k = x / (m * m - n * n)
        if k == math.floor(k):
            bc = k * 2 * m * n
            if math.gcd(m, n) == 1 and (m % 2) * (n % 2) == 0:
                total += max(min(a, bc - 1) - math.ceil(bc / 2) + 1, 0)
        # If bc = x
        k = x / (2 * m * n)
        if k == math.floor(k):
            bc = k * (m * m - n * n)
            if bc != x and math.gcd(m, n) == 1 and (m % 2) * (n % 2) == 0:
                total += max(min(a, bc - 1) - math.ceil(bc / 2) + 1, 0)
        # Advancement
        n += 1

i = 1
while True:
    enumerate_for_value(i)
    if i % 100 == 0:
        print((i, total))
    if total > 1000000:
        break
    i += 1
print(i)
