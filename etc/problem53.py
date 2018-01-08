#!/usr/bin/python3

a = [1]
b = []

count = 0

for i in range(1, 101):
    b = [0 for _ in range(0, i + 1)]
    b[0] = 1
    b[-1] = 1
    for j in range(1, len(a)):
        b[j] = a[j - 1] + a[j]
        if b[j] > 1000000:
            count += 1
    a, b = b, a

print(count)
