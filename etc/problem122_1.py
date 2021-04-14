
import math

UPPER_BOUND = 200

class Node:

    def __init__(self, value, parent):
        self.value = value
        self.parent = parent
        if self.parent is not None:
            self.value_set = self.parent.value_set | {self.value}
        else:
            self.value_set = {self.value}

    def __iter__(self):
        curr = self
        while curr is not None:
            yield curr.value
            curr = curr.parent

    def __contains__(self, value):
        return value in self.value_set

    def __len__(self):
        return len(self.value_set)

best_answers = [math.inf for _ in range(UPPER_BOUND)]
starting_point = Node(1, None)

def recurse(visited, n):
    if n > UPPER_BOUND:
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
            if i + j > UPPER_BOUND:
                continue
            if len(visited) - 1 >= best_answers[i + j - 1]:
                continue
            recurse(Node(i + j, visited), i + j)

recurse(starting_point, 1)
print(sum(best_answers))
