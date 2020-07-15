
import math

# Probably correct but too slow.

squares = {}

def is_square(x):
    if x not in squares:
        squares[x] = (frac(math.sqrt(x)) < 0.000000001)
    return squares[x]

def shortest(a, b, c):
    return is_square(a * a + (b + c) * (b + c))
    #return math.sqrt(a * a + (b + c) * (b + c))

def frac(x):
    return x - math.floor(x)

# ---
m = 10

for a in range(1, m + 1):
    for b in range(1, m + 1):
        for c in range(1, m + 1):
            if b <= c <= a and shortest(a, b, c):
                print((a, b + c))

exit(0)
# ---

m = 1
count = 0
while True:

    for b in range(1, m + 1):
        for c in range(b, m + 1):
            if shortest(m, b, c):
                count += 1

    if count > 1000000:
        break
    if m % 10 == 0:
        print(m, count)
    m += 1

print(m)
