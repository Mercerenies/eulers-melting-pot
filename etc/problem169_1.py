
# Using recurrence from
# https://en.wikipedia.org/wiki/Calkin%E2%80%93Wilf_tree#Stern's_diatomic_sequence,
# naive direct calculation.

from functools import cache


@cache
def fusc(n):
    if n in (0, 1):
        return n
    if n % 2 == 0:
        return fusc(n // 2)
    else:
        return fusc(n // 2) + fusc(n // 2 + 1)


def f(n):
    return fusc(n + 1)


print(f(10 ** 25))

# Editor's notes on AGSPL:
#
# * Can't use regular division because it's floating, so we implement
#   division by 2 ourselves.
#
# * Implementing the cache as an array of alists [[key, value], [key,
#   value], ...], since our Python implementation tells us there's
#   only about 200 values we need in total.
#
# * For loops are broken. We have to use an empty variable name "" or
#   the for loop will only work once, since the variable identifier in
#   the interpreter is a global and is never reset (so the names are
#   just concatenated together). This is also why there's always a
#   trailing space after the "f" keyword for 'for' loops. That's the
#   empty variable name.
