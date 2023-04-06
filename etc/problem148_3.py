
# Following the logic of the previous Python answers to this. For a
# given row n, the number of positions not divisible by 7 is equal to
# the product of (d+1) where d are the digits of n when written in
# base 7. This is a consequence of Kummer's theorem (write out what
# the carries look like).
#
# If our upper limit was a power of seven, then we could just use the
# triangular number formula and get a quick answer. But since our
# upper limit is 999,999,999 (33531600615 in base 7), we have to sort
# of "adjust" when we hit our upper bound.
#
# So we sum over everything and use triangle number formula whenever
# we can, getting a sort of recursive formula.

# Takes an argument written in base 7.
def count_up_to(n, p):
    if not n:
        return 1
    k = len(n)
    d = n.pop(0)
    return (d * (d + 1) // 2) * (p * (p + 1) // 2) ** (k - 1) + (d + 1) * count_up_to(n, p)

print(count_up_to([3, 3, 5, 3, 1, 6, 0, 0, 6, 1, 5], p=7)) # 999,999,999 in base 7
