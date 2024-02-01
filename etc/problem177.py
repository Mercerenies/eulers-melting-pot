
# Too slow :(

def check(a1, a2, b1, b2, c1, c2):
    c1 = 180 - a1 - b1 - b2
    d2 = 180 - b1 - a1 - a2
    d1 = a2 + b1 - c2
    p = 180 - a2 - b1

count = 0
for a2 in range(1, 180):
    print("a2 =", a2)
    for b1 in range(1, 180 - a2):  # (a2, b1, p) is a triangle, so sum must be 180.
        if a2 + b1 >= 360:
            continue
        for b2 in range(1, 180 - b1):  # (b1, b2) is an angle in convex polygon, so must be less than 180.
            if a2 + b1 + b2 >= 360:
                continue
            for a1 in range(1, 180 - a2):  # (a2, a1) is an angle in a convex polygon.
                c1 = 180 - a1 - b1 - b2
                if a2 + b1 + b2 + a1 + c1 >= 360 or c1 <= 0:
                    continue
                for c2 in range(1, 180 - c1):  # (c2, c1) is an angle in a convex polygon.
                    d1 = a2 + b1 - c2
                    d2 = 180 - b1 - a1 - a2
                    if d1 <= 0 or d2 <= 0:
                        continue
                    if a1 + a2 + b1 + b2 + c1 + c2 + d1 + d2 == 360:
                        print(a1, a2, b1, b2, c1, c2, d1, d2)
                        count += 1

print(count)
