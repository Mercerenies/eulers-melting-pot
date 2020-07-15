
import math

# b <= c <= a

# If the dimensions are (a, b, c), then the rotations are (c, a, b)
# and (b, c, a). We may assume we're always going in the a->b->c
# direction like in the diagram shown on projecteuler.net. Then one of
# the three rotations will always get the minimum path. In fact, it's
# easy to show (based on the formula I'll state in a minute) that this
# path is minimized in the rotation where b <= c <= a, so we can just
# limit ourselves to ordered triples satisfying that inequality.
#
# The distance along the path mentioned above is (by unfolding the
# cuboid) sqrt(a^2 + (b + c)^2). So we want to know when a^2 + (b +
# c)^2 is a perfect square. This is a problem of generating
# Pythagorean triples. We know, in particular, that a <= m, which
# helps us understand when to stop iteration. Also, if we're doing
# this incrementally and keeping a global running tally, then we only
# care about the case where a = m, since a < m was handled in an
# earlier iteration step. So once we know a = m, then we iterate every
# possible (b + c) value from 1 up to 2 a (can't be bigger than 2 a
# since b, c <= a), trying each possibility to see which ones give
# Pythagorean triples (a, b + c, ?). The ones that do get added into
# the running total (several times, for each possible (b, c) pair
# satisfying the inequalities above). Then sum it up.
#
# This solution runs in 0.8s on my machine.

total = 0

def enumerate_for_value(x):
    global total
    a = x
    for bc in range(1, 2 * a + 1):
        discr = math.sqrt(a * a + bc * bc)
        if discr == math.floor(discr):
            total += min(a, bc - 1) - math.ceil(bc / 2) + 1

i = 1
while True:
    enumerate_for_value(i)
    if total > 1000000:
        break
    i += 1
print(i)

# t = total
# i = i = a = x
# b = bc
# d = discr
# z = temporary variable
# y = temporary variable
# r = temporary variable
