
# Runs and solves in 5.5 minutes.
#
# Got it down to 1m27s by eliminating the inner d loop.

from dataclasses import dataclass
import math


@dataclass(frozen=True)
class Triangle:
    a: int
    b: int
    c: int

    def semiperimeter(self):
        return (self.a + self.b + self.c) / 2

    def inradius_squared(self):
        s = self.semiperimeter()
        return (s - self.a) * (s - self.b) * (s - self.c) / s


def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a


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
        if t.inradius_squared() > limit_squared:
            break
        k += 1
    return k


LIMIT = 1053779
LIMIT_SQUARED = LIMIT * LIMIT

var_bound = 1 + find_variable_bound(limit_squared=LIMIT_SQUARED)
#print(var_bound)

solutions = 0
for k in range(1, var_bound):
    #if k % 1000 == 0 or k < 1000:
    #    print(k)
    found_k_solutions = False
    for lam in range(1, var_bound):
        #if k == 1 and lam % 100 == 0:
        #    print(lam)
        if gcd(k, lam) != 1:
            continue
        if k < lam < 3 * k:
            continue
        if k == 1 and lam in (1, 3):
            # Claim: These are the only equilateral triangles produced
            # by this formula.
            #
            # Proof: Let (κ, λ, d) with gcd(κ, λ) = 1. Assume WLOG
            # that d = 1 (since d always produces similar triangles,
            # hence equilateral-ness is invariant under the value of
            # d).
            #
            # Then a = κ λ, b = (3 κ² + λ²) / 4, c = (2 κ λ + abs(3 κ²
            # - λ²) / 4. We assume the triangle is equilateral, hence
            # a = b = c.
            #
            # b = c gives 3 κ² + λ² = 2 κ λ + abs(3 κ² - λ²). Case
            # split on the result of the absolute value.
            #
            # Case 1: 3 κ² ≥ λ². Then 3 κ² + λ² = 2 κ λ + 3 κ² -
            # λ². So κ = λ. Since gcd = 1, this tells us κ = λ = 1.
            #
            # Case 2: 3 κ² < λ². Then 3 κ² + λ² = 2 κ λ + λ² - 3
            # κ². So 3 κ = λ. And thus, κ = 1 and λ = 3 (again, since
            # gcd = 1, this is the only possibility).
            #
            # Hence, these are the only two cases.
            continue
        d = 4 if (k + lam) % 2 == 1 else 1
        t = make_triangle(k, lam, d)
        r_squared = t.inradius_squared()
        if r_squared <= LIMIT_SQUARED:
            found_k_solutions = True
            solutions += math.floor(math.sqrt(LIMIT_SQUARED / r_squared))
            # We will double-count any solutions where d is divisible
            # by 3 but lam is not.
            #
            # Double count claim: If (k, lam, 3d) is a solution where
            # 3 does NOT divide lam, then (lam, 3k, d) is also a
            # solution.
            #
            # Proof: Basic algebra on the formula.
            #
            # Uniqueness claim: The above is the ONLY situation where
            # distinct (k, lam, d) satisfying the formula's conditions
            # can produce the same (a, b, c) side lengths.
            #
            # Proof: To be added, though I suspect it's just more
            # algebra to find a contradiction.
            if lam % 3 != 0:
                # Regardless of whether we're looking at multiples of
                # 1 or of 4, this calculation is the same. Every third
                # value of d gets double-counted.
                largest_d = math.floor(math.sqrt(LIMIT_SQUARED / r_squared))
                solutions -= largest_d // 3
        elif lam % 2 == 1:
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

'''
for k, v in solutions.items():
    if len(v) > 1:
        print(k, v)

for k in range(1, var_bound):
    max_lam = max((lam for k1, lam, _ in params if k1 == k), default=0)
    print(f"When k={k}, then max_lam={max_lam}, also {k * max_lam}")

ks = set(k for k, _, _, in params)
for k in range(1, 99999, 2):
    if k not in ks:
        print(k)
        break
'''

print(solutions)
