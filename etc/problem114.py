
cache = {};

def count_from_gray(left):
    if ('gray', left) in cache:
        return cache[('gray', left)]
    if left < -1:
        total = 0 # Impossible
    elif left == -1 or left == 0:
        # -1 case: there's no gray at the end, but it doesn't matter
        # -because there's no more red after it.
        total = 1
    else:
        total = 0
        for i in range(0, left - 2): # Min length of red = 3
            total += count_from_red(i)
        total += 1 # Add in the case of "everything else is gray"
    cache[('gray', left)] = total
    return total

def count_from_red(left):
    if ('red', left) in cache:
        return cache[('red', left)]
    if left < 0:
        total = 0 # Impossible
    elif left == 0:
        total = 1
    else:
        total = 0
        for i in range(-1, left): # Min length of gray = 1
            total += count_from_gray(i)
    cache[('red', left)] = total
    return total

print(count_from_gray(50))
