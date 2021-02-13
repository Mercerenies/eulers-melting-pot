
# Same as the previous solution, but doing it in a more
# dynamic-programming-esque order.

gray = [0 for _ in range(51)]
red  = [0 for _ in range(51)]

gray[0] = 1
red[0] = 1

for i in range(1, 51):
    g = 1
    for j in range(0, i - 2):
        g += red[j]
    gray[i] = g
    r = 1
    for j in range(0, i):
        r += gray[j]
    red[i] = r

print(gray[50])
