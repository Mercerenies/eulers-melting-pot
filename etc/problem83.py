
WIDTH, HEIGHT = 80, 80

arr = []

with open('./files/p083_matrix.txt', 'r') as f:
    for line in f:
        line = line.rstrip('\n')
        arr.append(list(map(int, line.split(','))))

# WIDTH, HEIGHT = 5, 5
# arr = [[131, 673, 234, 103,  18],
#        [201,  96, 342, 965, 150],
#        [630, 803, 746, 422, 111],
#        [537, 699, 497, 121, 956],
#        [805, 732, 524,  37, 331]]

# First, we do the same thing we did in 82, but with the knowledge
# that we start in the upper-left.
acc = list(map(lambda x: x[:], arr))

for y in range(1, HEIGHT):
    acc[y][0] += acc[y - 1][0]

for x in range(1, WIDTH):
    for y in range(0, HEIGHT):
        mincost = acc[y][x - 1] + arr[y][x]
        for y0 in range(HEIGHT):
            cost = acc[y0][x - 1]
            a = min(y, y0)
            b = max(y, y0)
            for curr in range(a, b + 1):
                cost += arr[curr][x]
            mincost = min(mincost, cost)
        acc[y][x] = mincost

# Next, we repeatedly allow left movements until we find a fixed
# point.
done = False
while not done:
    done = True
    for x in range(WIDTH - 2, 0, -1):
        for y in range(HEIGHT):
            if acc[y][x] > acc[y][x + 1] + arr[y][x]:
                acc[y][x] = acc[y][x + 1] + arr[y][x]
                done = False # Not a fixed point yet
    # Now do 82 again.
    for x in range(0, WIDTH):
        for y in range(0, HEIGHT):
            # Starting cost is the current cost or a movement from the
            # right, since we already have a viable route of that
            # cost.
            mincost = acc[y][x]
            if x > 0:
                mincost = min(mincost, acc[y][x - 1] + arr[y][x])
            # Downward movements
            for y0 in range(0, y):
                cost = acc[y0][x]
                for curr in range(y0 + 1, y + 1):
                    cost += arr[curr][x]
                mincost = min(mincost, cost)
            # Upward movements
            for y0 in range(y + 1, HEIGHT):
                cost = acc[y0][x]
                for curr in range(y, y0):
                    cost += arr[curr][x]
                mincost = min(mincost, cost)
            acc[y][x] = mincost

# Benchmark notes: This algorithm takes 15s and 8 iterations of the
# above loop to complete.

print(acc[HEIGHT - 1][WIDTH - 1])
