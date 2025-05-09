
# Some experiments with integer generators.

if __name__ == "__main__":
    values = [i ** 2 for i in range(1, 101)]
    four_transpositions = []
    x = 0
    for a in range(len(values)):
        for b in range(a + 1, len(values)):
            for c in range(b + 1, len(values)):
                for d in range(c + 1, len(values)):
                    a_sign = 1  # WLOG
                    b_sign = -1  # WLOG (currently: assuming exactly 2 positives and 2 negatives)
                    c_sign = -1
                    d_sign = 1
                    n = a_sign * values[a] + b_sign * values[b] + c_sign * values[c] + d_sign * values[d]
                    if n == 0:
                        print((a, a_sign), (b, b_sign), (c, c_sign), (d, d_sign))
                        x += 1
    print(x)
