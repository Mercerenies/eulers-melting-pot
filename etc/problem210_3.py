
# There are three cases. I'll deal with case (*) last because it's the
# most annoying.
#
# This version of the function only works for r divisible by 8, due to
# assumptions made. The other Scala files probably work for r
# divisible by 4 but nothing finer. Our actual input we care about is
# divisible by 8 so the distinction in moot in practice.
#
# 47 seconds in Python


def integer_pts_in_rect(a: int, b: int) -> int:
    return 2 * a * b - a - b + 1


# Case (**): Easy, just a rectangle
def region2(r: int) -> int:
    a = r // 4
    b = r // 2
    return 2 * (integer_pts_in_rect(a, b) + a + b - 1)


# Case (***): Easy, just a square
def region3(r: int) -> int:
    a = r // 2
    return 2 * (integer_pts_in_rect(a, a) + 2 * a - 1)


# Case (*): Circle centered around (r/8, r/8) w/ radius (r/8)*sqrt(2).
# The center point is irrelevant (as long as it's an integer, which
# for our input it is). So we just want to count integer points in the
# circle of radius (r/8)*sqrt(2), which is similar to the Gauss circle
# problem (https://en.wikipedia.org/wiki/Gauss_circle_problem) but we
# exclude boundary points. So we just count them by hand using a
# clever sort of edge-walking algorithm.
def region1(r: int) -> int:
    rad = r // 8
    total = 0
    total_on_diagonal = 0

    bound = 3 * rad // 2
    y = bound
    for x in range(0, bound + 1):
        if x < 100 or x % 1_000_000 == 0:
            print(x)
        # Find new boundary point
        lim = 2 * rad * rad - x * x
        while y > 0 and y * y >= lim:
            y -= 1
        # y is the largest point that works
        total += y
        # Note: `x > 0` to correct for origin (which is a degenerate
        # triangle and should not be counted)
        if y >= x > 0:
            total_on_diagonal += 1
        if y <= 0:
            break
    # We counted everything in Quadrant 1 (including one axis). Then
    # we multiply it by 4 to get the other quadrants. But this counted
    # points on the x = y line that we shouldn't have counted, so
    # subtract 2 * total_on_diagonal to get rid of those.
    return 4 * total - 2 * total_on_diagonal


def n(r: int) -> int:
    return region1(r) + region2(r) + region3(r)


#for i in range(8, 100, 8):
#    print(f"n({i}) = {n(i)}")
print(n(1_000_000_000))
