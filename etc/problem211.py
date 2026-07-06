
# Most naive possible approach: A giant array
#
# 6 minutes

LIMIT = 64_000_000


sums_of_squares = [1 for _ in range(LIMIT)]

biggest = 0

for i in range(2, LIMIT):
    #if i < 100 or i % 10_000 == 0:
    #    print(i)
    k = 1
    while i * k < LIMIT:
        sums_of_squares[i * k] += i * i
        biggest = max(biggest, sums_of_squares[i * k])
        k += 1

all_perfect_squares = set()
i = 0
while i * i <= biggest:
    all_perfect_squares.add(i * i)
    i += 1

all_of_them = [n for n, sigma in enumerate(sums_of_squares) if sigma in all_perfect_squares]
total_count = sum(n for n, sigma in enumerate(sums_of_squares) if sigma in all_perfect_squares)
print(all_of_them)
print(total_count)
