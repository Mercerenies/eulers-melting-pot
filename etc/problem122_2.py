
best_answers = [300 for _ in range(200)]
starting_point = [1]

def recurse(visited, n):
    if n > 200:
        return
    best_answers[n-1] = min(best_answers[n-1], len(visited)-1)
    for i in visited:
        for j in visited:
            if i > j:
                continue
            if i + j in visited:
                continue
            if i + j <= n:
                continue
            if i + j > 200:
                continue
            if len(visited) - 1 >= best_answers[i + j - 1]:
                continue
            recurse([i + j] + visited, i + j)

recurse(starting_point, 1)
print(sum(best_answers))
