
def f(n, m):
    gray = [0 for _ in range(n+1)]
    red  = [0 for _ in range(n+1)]

    gray[0] = 1
    red[0] = 1

    for i in range(1, n+1):
        g = 1
        for j in range(0, i - (m-1)):
            g += red[j]
        gray[i] = g
        r = 1
        for j in range(0, i):
            r += gray[j]
        red[i] = r

    return gray[n]

m = 50
n = 1
while f(n, m) <= 1000000:
    n += 1
print(n)
