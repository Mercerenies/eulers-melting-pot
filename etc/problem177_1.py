
# Straight brute force, 4 minutes in Python

from math import sin, cos, sqrt, asin, degrees, radians


def clamp(value, lower, upper):
    if value < lower:
        return lower
    elif value > upper:
        return upper
    else:
        return value


class Quadrilateral:

    def __init__(self, *angles):
        assert len(angles) == 8
        assert sum(angles) == 360
        # Store the quadrilateral as a set of tuples indicating all
        # rotations and reflections, so we can avoid duplicates. Very
        # inefficient representation, but it's good enough for now.
        self.angles = set()
        for _ in range(4):
            self.angles.add(angles)
            self.angles.add(tuple(reversed(angles)))
            angles = angles[-2:] + angles[:-2]
        self.angles = frozenset(self.angles)

    def __repr__(self):
        angles = next(iter(self.angles))
        return f"Quadrilateral({','.join(str(x) for x in angles)})"

    def __eq__(self, other):
        if not isinstance(other, Quadrilateral):
            return False
        return self.angles == other.angles

    def __hash__(self):
        return hash(("Quadrilateral", self.angles))


EPSILON = 10e-9


def is_integer(v):
    return abs(v - round(v)) < EPSILON


count = 0
all_solutions = set()

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
                l = sin(radians(b2)) * k / sin(radians(b1))
                m = sin(radians(a2)) * l / sin(radians(a1))
                n = sin(radians(d2)) * m / sin(radians(d1))
                x = sqrt(k ** 2 + n ** 2 - 2 * k * n * cos(radians(c3)))
                inner = clamp(n * sin(radians(c3)) / x, -1, 1)
                c1 = degrees(asin(inner))
                inner = clamp(k * sin(radians(c3)) / x, -1, 1)
                c2 = degrees(asin(inner))
                if is_integer(c1) and is_integer(c2):
                    int_c1 = int(c1 + 0.5)
                    int_c2 = int(c2 + 0.5)
                    if b2 + int_c1 >= 180 or int_c2 + d1 >= 180:
                        # Concave
                        continue
                    try:
                        q = Quadrilateral(a1, a2, b1, b2, int_c1, int_c2, d1, d2)
                    except AssertionError:
                        continue
                    all_solutions.add(q)
                    count += 1

print(len(all_solutions))
print(count)
