
UPPER = 50

total = 0
for x1 in range(0, UPPER + 1):
    for y1 in range(0, UPPER + 1):
        for x2 in range(0, UPPER + 1):
            for y2 in range(0, UPPER + 1):
                if (x1, y1) == (x2, y2) or (x1, y1) == (0, 0) or (x2, y2) == (0, 0):
                    continue
                d1 = x1 * x1 + y1 * y1
                d2 = x2 * x2 + y2 * y2
                d3 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
                a, b, c = sorted([d1, d2, d3])
                if a + b == c:
                    total += 1
print(int(total / 2))
