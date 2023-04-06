
# Do the base 7 thing w/o the intermediate list.
def fast_count(n):
    total = 1
    while n != 0:
        total *= ((n % 7) + 1)
        n //= 7
    return total

def run():
    total = 0
    for a in range(1, 8):
        for b in range(1, 8):
            for c in range(1, 8):
                print(a, b, c)
                for d in range(1, 8):
                    for e in range(1, 8):
                        for f in range(1, 8):
                            for g in range(1, 8):
                                for h in range(1, 8):
                                    for i in range(1, 8):
                                        for j in range(1, 8):
                                            for k in range(1, 8):
                                                if (a, b, c, d, e, f, g, h, i, j, k) == (4, 4, 6, 4, 2, 7, 1, 1, 7, 2, 7):
                                                    return total
                                                total += a * b * c * d * e * f * g * h * i * j * k
    raise RuntimeError("Oops")

print(run())
