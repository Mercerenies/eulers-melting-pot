
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
