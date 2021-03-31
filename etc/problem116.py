
def f(n, c):
    gray = [0 for _ in range(n+1)]
    col  = [0 for _ in range(n+1)]

    gray[0] = 1
    col[0] = 1

    for i in range(1, n+1):
        g = col[i - c] if i - c >= 0 else 0
        gray[i] = g

        r = 0
        for j in range(i):
            r += gray[j]
        col[i] = r + g

    return col[n] - 1

print(f(50, 2) + f(50, 3) + f(50, 4))
