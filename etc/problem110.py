
# Same algorithm as problem108_1.py

def is_prime(x):
    for i in range(2, x - 1):
        if x % i == 0:
            return False
    return True

# Same song different verse. This time, we need 15 primes to be assured of our solution.
def enumerate_primes():
    return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]

def number_from_exps(exps):
    value = 1
    for p, x in zip(enumerate_primes(), exps):
        value *= p ** x
    return value

# Want this to be > 8,000,000 (we don't divide it by 2)
def count_solutions(exp):
    prod = 1
    for power in exp:
        prod *= 2 * power + 1
    return prod + 1

best = None

def recurse(acc):
    global best
    # First, have we already found a solution?
    if count_solutions(acc) > 8000000:
        solution = number_from_exps(acc)
        if best is None:
            best = solution
        best = min(best, solution)
        return
    # Otherwise, recurse
    if len(acc) == 0:
        stopping_condition = 4000000 # Far enough for the starting condition (prime = 2)
    else:
        stopping_condition = acc[-1] + 1
    acc.append(None)
    old_best = best
    for i in range(1, stopping_condition):
        acc[-1] = i
        recurse(acc)
        # Short-circuiting; makes it much faster :)
        if best is not None and number_from_exps(acc) > best:
            break
    del acc[-1]

recurse([])
print(best)
