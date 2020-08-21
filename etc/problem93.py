
import operator
import itertools

class Dummy:
    def __add__(self, other):
        return self
    def __radd__(self, other):
        return self
    def __sub__(self, other):
        return self
    def __rsub__(self, other):
        return self
    def __mul__(self, other):
        return self
    def __rmul__(self, other):
        return self
    def __truediv__(self, other):
        return self
    def __rtruediv__(self, other):
        return self

def div(a, b):
    if b == 0:
        return Dummy()
    return a / b

OPS = [operator.add, operator.sub, operator.mul, div]
EPSILON = 0.0001

def all_trees(n):
    if n == 1:
        yield lambda x: x
    else:
        for i in range(1, n):
            j = n - i
            for lhs in all_trees(i):
                for rhs in all_trees(j):
                    for op in OPS:
                        def tmp(*args, lhs=lhs, rhs=rhs, op=op):
                            x = lhs(*args[:i])
                            y = rhs(*args[i:])
                            return op(x, y)
                        yield tmp

def solve_values(a, b, c, d):
    values = {}
    for tree in all_trees(4):
        for perm in itertools.permutations((a, b, c, d)):
            curr = tree(*perm)
            if not isinstance(curr, Dummy) and curr - int(curr) < EPSILON:
                curr = int(curr)
                values[curr] = 1
    for i in itertools.count(1):
        if i not in values:
            return i

def solve_all():
    for a in range(1, 7):
        for b in range(a + 1, 8):
            for c in range(b + 1, 9):
                for d in range(c + 1, 10):
                    yield a, b, c, d

result = max(solve_all(), key=lambda x: solve_values(*x))
print(''.join(map(str, result)))
