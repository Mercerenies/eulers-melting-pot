
# Direct translation of problem180.rb into Python, so we can Pyf**k it.
#
# This program runs in 7-ish seconds.
#
# Original plan was to double encode this. First, to get an exec(),
# then to eval() the exec(). That turned out to be far too large and
# exceeded Python's recursion limit by a lot. Plan B, now, is to try
# to get everything to be exec-able.

from fractions import Fraction

solutions = set()
k = 35
sqrts = {k * k: k for k in range(k+1)}


def sqrt(r):
    n = sqrts.get(r.numerator)
    d = sqrts.get(r.denominator)
    if None in (n, d):
        return None
    return Fraction(n, d)


def in_bounds(z):
    return z and 1 <= z.numerator < z.denominator <= k


def produce_solutions_for(x, y):
    z = x + y
    if in_bounds(z):
        solutions.add(x + y + z)
    z = sqrt(x * x + y * y)
    if in_bounds(z):
        solutions.add(x + y + z)
    z = x * y / (x + y)
    if in_bounds(z):
        solutions.add(x + y + z)
    z = sqrt((x * y) ** 2 / (x * x + y * y))
    if in_bounds(z):
        solutions.add(x + y + z)


def each_fraction():
    for a in range(1, k + 1):
        for b in range(a + 1, k + 1):
            yield Fraction(a, b)


for x in each_fraction():
    for y in each_fraction():
        produce_solutions_for(x, y)

final_total = sum(solutions)
print(final_total.numerator + final_total.denominator)
