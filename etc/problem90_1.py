
# Restricted syntax form

# Allowed syntax:
# one-line functions
# if expressions (optional else, defaults to None)
# true, false
# integer literals
# = (comparison)
# +, -, *
# local vars
# function calls (unqualified)

def leq(x, y):
    return abs(y - x) == (y - x)

def not_(a):
    return False if a else True

def do_10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9):
    # Don't.... don't even ask
    return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9

def div2(a):
    return a * pow(2, 0-1)

# DO NOT EXPLICITLY PASS X=9 TO THIS FUNCTION DON'T DO IT PLEASE
def at(a, b, c, d, e, f, x):
    return True if x == a else True if x == b else True if x == c else True if x == d else True if x == e else True if x == f else at(a, b, c, d, e, f, 9) if x == 6 else False

def ateither(a, b, c, d, e, f, g, h, i, j, k, l, x, y):
    return (True if at(g, h, i, j, k, l, y) else (at(g, h, i, j, k, l, x) if at(a, b, c, d, e, f, y) else False)) if at(a, b, c, d, e, f, x) else (at(g, h, i, j, k, l, x) if at(a, b, c, d, e, f, y) else False)

def dice1():
    return do_10(dice2(0), dice2(1), dice2(2), dice2(3), dice2(4), dice2(5), dice2(6), dice2(7), dice2(8), dice2(9))

def dice2(a):
    return do_10(dice3(a, 0), dice3(a, 1), dice3(a, 2), dice3(a, 3), dice3(a, 4), dice3(a, 5), dice3(a, 6), dice3(a, 7), dice3(a, 8), dice3(a, 9))
def dice3(a, b):
    return 0 if leq(b, a) else do_10(dice4(a, b, 0), dice4(a, b, 1), dice4(a, b, 2), dice4(a, b, 3), dice4(a, b, 4), dice4(a, b, 5), dice4(a, b, 6), dice4(a, b, 7), dice4(a, b, 8), dice4(a, b, 9))

def dice4(a, b, c):
    return 0 if leq(c, b) else do_10(dice5(a, b, c, 0), dice5(a, b, c, 1), dice5(a, b, c, 2), dice5(a, b, c, 3), dice5(a, b, c, 4), dice5(a, b, c, 5), dice5(a, b, c, 6), dice5(a, b, c, 7), dice5(a, b, c, 8), dice5(a, b, c, 9))

def dice5(a, b, c, d):
    return 0 if leq(d, c) else do_10(dice6(a, b, c, d, 0), dice6(a, b, c, d, 1), dice6(a, b, c, d, 2), dice6(a, b, c, d, 3), dice6(a, b, c, d, 4), dice6(a, b, c, d, 5), dice6(a, b, c, d, 6), dice6(a, b, c, d, 7), dice6(a, b, c, d, 8), dice6(a, b, c, d, 9))

def dice6(a, b, c, d, e):
    return 0 if leq(e, d) else do_10(dice7(a, b, c, d, e, 0), dice7(a, b, c, d, e, 1), dice7(a, b, c, d, e, 2), dice7(a, b, c, d, e, 3), dice7(a, b, c, d, e, 4), dice7(a, b, c, d, e, 5), dice7(a, b, c, d, e, 6), dice7(a, b, c, d, e, 7), dice7(a, b, c, d, e, 8), dice7(a, b, c, d, e, 9))

def dice7(a, b, c, d, e, f):
    return 0 if leq(f, e) else do_10(dice8(a, b, c, d, e, f, 0), dice8(a, b, c, d, e, f, 1), dice8(a, b, c, d, e, f, 2), dice8(a, b, c, d, e, f, 3), dice8(a, b, c, d, e, f, 4), dice8(a, b, c, d, e, f, 5), dice8(a, b, c, d, e, f, 6), dice8(a, b, c, d, e, f, 7), dice8(a, b, c, d, e, f, 8), dice8(a, b, c, d, e, f, 9))

def dice8(a, b, c, d, e, f, g):
    return do_10(dice9(a, b, c, d, e, f, g, 0), dice9(a, b, c, d, e, f, g, 1), dice9(a, b, c, d, e, f, g, 2), dice9(a, b, c, d, e, f, g, 3), dice9(a, b, c, d, e, f, g, 4), dice9(a, b, c, d, e, f, g, 5), dice9(a, b, c, d, e, f, g, 6), dice9(a, b, c, d, e, f, g, 7), dice9(a, b, c, d, e, f, g, 8), dice9(a, b, c, d, e, f, g, 9))

def dice9(a, b, c, d, e, f, g, h):
    return 0 if leq(h, g) else do_10(dice10(a, b, c, d, e, f, g, h, 0), dice10(a, b, c, d, e, f, g, h, 1), dice10(a, b, c, d, e, f, g, h, 2), dice10(a, b, c, d, e, f, g, h, 3), dice10(a, b, c, d, e, f, g, h, 4), dice10(a, b, c, d, e, f, g, h, 5), dice10(a, b, c, d, e, f, g, h, 6), dice10(a, b, c, d, e, f, g, h, 7), dice10(a, b, c, d, e, f, g, h, 8), dice10(a, b, c, d, e, f, g, h, 9))

def dice10(a, b, c, d, e, f, g, h, i):
    return 0 if leq(i, h) else do_10(dice11(a, b, c, d, e, f, g, h, i, 0), dice11(a, b, c, d, e, f, g, h, i, 1), dice11(a, b, c, d, e, f, g, h, i, 2), dice11(a, b, c, d, e, f, g, h, i, 3), dice11(a, b, c, d, e, f, g, h, i, 4), dice11(a, b, c, d, e, f, g, h, i, 5), dice11(a, b, c, d, e, f, g, h, i, 6), dice11(a, b, c, d, e, f, g, h, i, 7), dice11(a, b, c, d, e, f, g, h, i, 8), dice11(a, b, c, d, e, f, g, h, i, 9))

def dice11(a, b, c, d, e, f, g, h, i, j):
    return 0 if leq(j, i) else do_10(dice12(a, b, c, d, e, f, g, h, i, j, 0), dice12(a, b, c, d, e, f, g, h, i, j, 1), dice12(a, b, c, d, e, f, g, h, i, j, 2), dice12(a, b, c, d, e, f, g, h, i, j, 3), dice12(a, b, c, d, e, f, g, h, i, j, 4), dice12(a, b, c, d, e, f, g, h, i, j, 5), dice12(a, b, c, d, e, f, g, h, i, j, 6), dice12(a, b, c, d, e, f, g, h, i, j, 7), dice12(a, b, c, d, e, f, g, h, i, j, 8), dice12(a, b, c, d, e, f, g, h, i, j, 9))

def dice12(a, b, c, d, e, f, g, h, i, j, k):
    return 0 if leq(k, j) else do_10(dice13(a, b, c, d, e, f, g, h, i, j, k, 0), dice13(a, b, c, d, e, f, g, h, i, j, k, 1), dice13(a, b, c, d, e, f, g, h, i, j, k, 2), dice13(a, b, c, d, e, f, g, h, i, j, k, 3), dice13(a, b, c, d, e, f, g, h, i, j, k, 4), dice13(a, b, c, d, e, f, g, h, i, j, k, 5), dice13(a, b, c, d, e, f, g, h, i, j, k, 6), dice13(a, b, c, d, e, f, g, h, i, j, k, 7), dice13(a, b, c, d, e, f, g, h, i, j, k, 8), dice13(a, b, c, d, e, f, g, h, i, j, k, 9))

def dice13(a, b, c, d, e, f, g, h, i, j, k, l):
    return 0 if leq(l, k) else (
        (
            (
                (
                    (
                        (
                            (
                                (
                                    (
                                        (
                                            1
                                        ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 8, 1) else 0
                                    ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 6, 4) else 0
                                ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 4, 6) else 0
                            ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 3, 6) else 0
                        ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 2, 5) else 0
                    ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 1, 6) else 0
                ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 0, 6) else 0
            ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 0, 4) else 0
        ) if ateither(a, b, c, d, e, f, g, h, i, j, k, l, 0, 1) else 0
    )

def dbg(*args, **kwargs):
    print(args, kwargs)
    return 0

print(div2(dice1()))
