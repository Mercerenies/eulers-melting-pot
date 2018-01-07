#!/usr/bin/python3

i = 1
while True:
    x = list(str(i * 1))
    y = list(str(i * 2))
    z = list(str(i * 3))
    t = list(str(i * 4))
    u = list(str(i * 5))
    v = list(str(i * 6))
    x.sort()
    y.sort()
    z.sort()
    t.sort()
    u.sort()
    v.sort()
    if x == y == z == t == u == v:
        print(i)
        break
    i += 1
