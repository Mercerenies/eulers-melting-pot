
# Here we go...

def is_prime(x):
    for i in range(2, x - 1):
        if x % i == 0:
            return False
    return True

#def enumerate_primes():
#    i = 2
#    while True:
#        if is_prime(i):
#            yield i
#        i += 1

# By direct calculation, we can see that the prime product
# 2*3*5*7*11*13*17 = 510510 is a solution. Any solution which uses a
# prime bigger than 17 is objectively worse than one that doesn't (a
# solution either has some 0 prime exponents which should be shifted
# away, or the rightmost exponents can be eliminated to produce
# 510510). So the only primes we care about are the first seven.
def enumerate_primes():
    return [2, 3, 5, 7, 11, 13, 17]

def number_from_exps(exps):
    value = 1
    for p, x in zip(enumerate_primes(), exps):
        value *= p ** x
    return value

# Want this to be > 2000 (we don't divide it by 2)
def count_solutions(exp):
    prod = 1
    for power in exp:
        prod *= 2 * power + 1
    return prod + 1

best = None

def recurse(acc):
    global best
    # First, have we already found a solution?
    if count_solutions(acc) > 2000:
        solution = number_from_exps(acc)
        if best is None:
            best = solution
        best = min(best, solution)
        return
    # Otherwise, recurse
    if len(acc) == 0:
        stopping_condition = 1000 # Far enough for the starting condition (prime = 2)
    else:
        stopping_condition = acc[-1] + 1
    acc.append(None)
    for i in range(1, stopping_condition):
        acc[-1] = i
        recurse(acc)
    del acc[-1]

recurse([])
print(best)
