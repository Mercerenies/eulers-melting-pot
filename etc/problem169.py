
# Conclusion: It's called Stern's diatomic sequence (A002487)

# Calculates f(n) recursively. n is the number. exponent is the
# exponent of the largest power of 2 under consideration. exponent is
# initially the largest k such that 2^k <= n, but it can end up
# smaller. used_once indicates whether or not we've already used this
# power of 2 once. Returns the number of sums.
def go(n, exponent, used_once):
    if n == 0:
        # Base case.
        return 1
    if exponent < 0:
        # We have failed. Backtrack.
        return 0

    power = 2 ** exponent
    if power > n:
        # Power is too big, go smaller.
        return go(n, exponent - 1, False)

    # We must either include power or power // 2. Try each one in turn.
    result = 0
    # Include power
    if used_once:
        result += go(n - power, exponent - 1, False)
    else:
        result += go(n - power, exponent, True)
    # Include power - 1
    result += go(n - power // 2, exponent - 1, True)
    return result


def f(n):
    exponent = 0
    while 2 ** (exponent + 1) <= n:
        exponent += 1
    return go(n, exponent, False)


for i in range(1, 100):
    print(i, f(i), sep="\t")
