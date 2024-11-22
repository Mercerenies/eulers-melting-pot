
from dataclasses import dataclass
from collections import defaultdict


@dataclass(frozen=True)
class Triangle:
    a: int
    b: int
    c: int

    def is_equilateral(self):
        return self.a == self.b == self.c


def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a


def semiperimeter(a, b, c):
    return (a + b + c) / 2


def inradius_squared(a, b, c):
    s = semiperimeter(a, b, c)
    return (s - a) * (s - b) * (s - c) / s


def make_triangle(k, lam, d):
    return Triangle(
        a=d * k * lam,
        b=d * (3 * k * k + lam * lam) // 4,
        c=d * (2 * k * lam + abs(3 * k * k - lam * lam)) // 4,
    )


def find_variable_bound(limit_squared):
    # Find bounds for k and lam. We encounter extreme values for k
    # when lam == 1 and vice versa, so the largest that either k or
    # lam can be is the largest ODD value n such that (k=n, lam=1,
    # d=1) has the inradius we're looking for.
    k = 1
    while True:
        t = make_triangle(k, 1, 1)
        if inradius_squared(t.a, t.b, t.c) > limit_squared:
            break
        k += 1
    return k


LIMIT = 1000
LIMIT_SQUARED = LIMIT * LIMIT

var_bound = 1 + find_variable_bound(limit_squared=LIMIT_SQUARED)
print(var_bound)

solutions = defaultdict(lambda: [])
for k in range(1, var_bound):
    #print(k)
    found_k_solutions = False
    for lam in range(1, var_bound):
        #if k == 1 and lam % 100 == 0:
        #    print(lam)
        if gcd(k, lam) != 1:
            continue
        if k < lam < 3 * k:
            continue
        if make_triangle(k, lam, 4).is_equilateral():
            # Seems to only occur when k = 1 and lam in (1, 3). See if
            # we can prove that these are the only cases?
            continue

        dd = 4 if (k + lam) % 2 == 1 else 1
        d = 0
        while True:
            d += dd
            t = make_triangle(k, lam, d)
            r_squared = inradius_squared(t.a, t.b, t.c)
            if r_squared <= LIMIT_SQUARED:
                solutions[t].append((k, lam, d))
                found_k_solutions = True
            else:
                break
        if d == dd and lam % 2 == 1:
            # If lam is odd and we found no solutions, then we never
            # will again (Note: this seems experimentally true but I
            # haven't proven it; why does this work?)
            break
    #print(k, lam)
    if not found_k_solutions and k % 2 == 1:
        # If k is odd and we found no solutions, then we never will
        # again (Note: this seems experimentally true but I haven't
        # proven it; why does this work?)
        break

for k, v in solutions.items():
    if len(v) > 1:
        print(k, v)


'''
for k in range(1, var_bound):
    max_lam = max((lam for k1, lam, _ in params if k1 == k), default=0)
    print(f"When k={k}, then max_lam={max_lam}, also {k * max_lam}")

ks = set(k for k, _, _, in params)
for k in range(1, 99999, 2):
    if k not in ks:
        print(k)
        break
'''

print(len(solutions))
