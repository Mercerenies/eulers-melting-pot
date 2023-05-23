
# Straightforward brute force with caching. 52 seconds.

from functools import cache

def single_digit_sum(x):
    total = 0
    while x != 0:
        total += x % 10
        x //= 10
    return total

@cache
def digital_sum(x):
    if x < 10:
        return x
    else:
        return digital_sum(single_digit_sum(x))

@cache
def digit_root_sum(x):
    # Find all of the ways to split x into specifically two parts, and
    # add the digit root sums of those.
    i = 2
    best = 0
    while i * i <= x:
        if x % i == 0:
            j = x // i
            best = max(best, digit_root_sum(i) + digit_root_sum(j))
        i += 1
    return max(best, digital_sum(x))


s = 0
for i in range(2, 1000000):
    s += digit_root_sum(i)
print(s)
