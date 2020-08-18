
def all_dice():
    for a in range(0, 10):
        for b in range(a+1, 10):
            for c in range(b+1, 10):
                for d in range(c+1, 10):
                    for e in range(d+1, 10):
                        for f in range(e+1, 10):
                            yield [a, b, c, d, e, f]

def all_pairs():
    for a in all_dice():
        for b in all_dice():
            if a <= b:
                yield [a, b]

def makes_sense(d, n):
    if n in d:
        return True
    if n == 6 and 9 in d:
        return True
    if n == 9 and 6 in d:
        return True
    return False

def can_show_impl(dice, number):
    for d, n in zip(dice, number):
        if not makes_sense(d, n):
            return False
    return True

def can_show(dice, number):
    return can_show_impl(dice, number) or can_show_impl(reversed(dice), number)

squares = [[0, 1], [0, 4], [0, 9], [1, 6], [2, 5], [3, 6], [4, 9], [6, 4], [8, 1]]

total = 0
for d in all_pairs():
    if all(map(lambda x: can_show(d, x), squares)):
        total += 1
print(total)
