
# Trying to understand one of the algorithms for graph coloring via
# dynamic programming, so I'm breaking it down in Python.

# Python w/ dynamic programming: 1m 9sec

def remove_trailing_zeroes(lst):
    # For pretty-printing purposes
    res = list(filter(lambda x: x != 0, reversed(lst)))
    res.reverse()
    return res


ROW_COUNT = 8


downward_tri_colorings = [1, 1, 1]  # a
for n in range(2, ROW_COUNT + 1):
    new_row_colorings = [0] * (3 ** n)  # b
    for i in range(3 ** (n - 1)):
        if i % 100 == 0:
            print(n, i)
        for j in range(3 ** n):
            prod = 1
            c = i  # Previous row Δ info.
            d = j  # Current row Δ info.
            for k in range(1, n):  # Iterate over current row ∇'s.
                # Assuming we are in scenario i for the previous row
                # and scenario j for the next row, ask whether the ∇
                # at index k can be red, green, or blue.
                x = [1, 1, 1]
                x[c % 3] = 0  # It can't be the color of the above Δ
                x[d % 3] = 0  # It can't be the color of the right Δ
                x[(d // 3) % 3] = 0  # It can't be the color of the
                # left Δ Now shift color information to the left by
                # one, so we can consider all of the other colors for
                # the other ∇'s.
                c //= 3
                d //= 3
                prod *= sum(x)
            new_row_colorings[j] += downward_tri_colorings[i] * prod
    downward_tri_colorings = new_row_colorings

print(sum(downward_tri_colorings))
