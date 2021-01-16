
# Okay, here we go. We want to know when
#
# 1/x + 1/y = 1/n
#
# Assume x <= y, to avoid counting a solution twice. This is
# equivalent (y = wx) to saying
#
# 1/x + 1/wx = 1/n
#
# With w >= 1. Solve for x:
#
# x = n(w + 1)/w
#
# Then we have
#
# x = n(w + 1)/w
# y = n(w + 1)
#
# And we have a solution whenever the two are integers. y is an
# integer when the denominator of w divides n, and x is an integer
# when the numerator of w divides n. So the set of solutions is in
# bijection with the set of fractions (a/b) where a and b divide n and
# b <= a.
#
# That algorithm gives us enough terms to look up the sequence in
# OEIS. Lo and behold, it's A018892, and there's a convenient formula
# right there.

import math

def get_prime_exponents(n):
    i = 2
    while n > 1:
        if n % i == 0:
            power = 1
            ii = i
            while n % ii == 0:
                power += 1
                ii *= i
            power -= 1
            n /= (ii / i)
            yield power
        i += 1

def count_solutions(n):
    prod = 1
    for power in get_prime_exponents(n):
        prod *= (2 * power + 1)
    return (prod + 1) / 2

n = 2
while count_solutions(n) <= 1000:
    if n % 10000 == 0:
        print(n)
    n += 1
print(n)
