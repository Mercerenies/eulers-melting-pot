
# Fix p=7 and optimize where possible from problem148_3.py
def count_up_to(n):
    if not n:
        return 1
    k = len(n)
    d = n.pop(0)

    t = 0
    for i in range(d + 1):
        t += i

    return t * 28 ** (k - 1) + (d + 1) * count_up_to(n)

print(count_up_to([3, 3, 5, 3, 1, 6, 0, 0, 6, 1, 5])) # 999,999,999 in base 7

# Now try doing it in reverse to avoid the recursion.
total = 1
b = 1
for d in [5, 1, 6, 0, 0, 6, 1, 3, 5, 3, 3]:

    t = 0
    for i in range(d + 1):
        t += i

    total *= (d + 1)
    total += t * b
    b *= 28

print(total)

# Translations for TRANSCRIPT:
#
# total = thumbtack
# b = beret
# d = dagger
# i = icebox
# t = table
# temporary variable 1 = paper
# constant 0 = xylophone
# constant 28 = carrot
