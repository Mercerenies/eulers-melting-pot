
# problem177_2.py but without the custom data structure. Everything is
# just lists, tuples, integers, and prayers.
#
# Bad idea; 9 minutes in Python means it'll never run in anything
# else. And it's not even fully right.


from math import sin, cos, sqrt, asin, degrees, radians


LARGE_PRIME = 10000019


sines = []
cosines = []

for i in range(180):
    sines.append(sin(radians(i)))
    cosines.append(cos(radians(i)))


def clamp(value, lower, upper):
    if value < lower:
        return lower
    elif value > upper:
        return upper
    else:
        return value


def quad_rotate(quad):
    return quad[-2:] + quad[:-2]


def quad_reflect(quad):
    return tuple(reversed(quad))


def quad_ordered_hash(quadrilateral):
    """Hash that only considers identical quadrilaterals. Does NOT
    consider rotations or reflections."""
    h = 0
    for q in quadrilateral:
        h = (h * 180 + q) % LARGE_PRIME
    return h


def quad_hash(quadrilateral):
    h = 0
    for _ in range(4):
        h ^= quad_ordered_hash(quadrilateral)
        h ^= quad_ordered_hash(quad_reflect(quadrilateral))
        quadrilateral = quad_rotate(quadrilateral)
    return h % LARGE_PRIME


def quad_equal(q1, q2):
    for _ in range(4):
        if q1 == q2 or q1 == quad_reflect(q2):
            return True
        q1 = quad_rotate(q1)
    return False


EPSILON = 10e-9


def is_integer(v):
    return abs(v - round(v)) < EPSILON


all_solutions = [[] for _ in range(LARGE_PRIME)]

for a1 in range(1, 180):
    if a1 % 10 == 0:
        print(f"a1 = {a1}")
    for a2 in range(1, 180 - a1):
        for b1 in range(1, 180):
            if a2 + b1 >= 180:
                # Concave
                continue
            b2 = 180 - b1 - a1 - a2
            if b2 <= 0 or b2 >= 180:
                continue
            for d2 in range(1, 180):
                d1 = 180 - d2 - a1 - a2
                if d1 <= 0 or d1 >= 180:
                    continue
                if d2 + a1 >= 180:
                    # Concave
                    continue
                # Now the hard angles: c1 and c2
                c3 = 180 - a1 - a2
                k = 1  # We don't care about similar figures, so assert k = 1
                l = sines[b2] * k / sines[b1]
                m = sines[a2] * l / sines[a1]
                n = sines[d2] * m / sines[d1]
                x = sqrt(k ** 2 + n ** 2 - 2 * k * n * cosines[c3])
                inner = clamp(n * sines[c3] / x, -1, 1)
                c1 = degrees(asin(inner))
                inner = clamp(k * sines[c3] / x, -1, 1)
                c2 = degrees(asin(inner))
                if is_integer(c1) and is_integer(c2):
                    int_c1 = int(c1 + 0.5)
                    int_c2 = int(c2 + 0.5)
                    if b2 + int_c1 >= 180 or int_c2 + d1 >= 180:
                        # Concave
                        continue
                    if a1 + a2 + b1 + b2 + int_c1 + int_c2 + d1 + d2 != 360:
                        # Not a quadrilateral
                        continue
                    h = quad_hash([a1, a2, b1, b2, int_c1, int_c2, d1, d2])
                    bucket = all_solutions[h]
                    is_duplicate = False
                    for existing in bucket:
                        if quad_equal(existing, [a1, a2, b1, b2, int_c1, int_c2, d1, d2]):
                            # Found a duplicate
                            is_duplicate = True
                            break
                    if not is_duplicate:
                        bucket.append([a1, a2, b1, b2, int_c1, int_c2, d1, d2])

count = 0
for bucket in all_solutions:
    count += len(bucket)
print(count)

