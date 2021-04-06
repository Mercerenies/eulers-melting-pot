
def get(arr, n):
    if n < 0:
        return 0
    else:
        return arr[n]

def f(n):
    gray = [0 for _ in range(n+1)]
    col  = [0 for _ in range(n+1)]

    gray[0] = 1
    col[0] = 1

    for i in range(1, n+1):
        g = get(col, i - 2) + get(col, i - 3) + get(col, i - 4)
        gray[i] = g

        r = 0
        for j in range(i):
            r += gray[j]
        col[i] = r + g

    return col[n] - 1

print(f(50) + 1) # Plus one to account for empty case
