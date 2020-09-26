
# Testing solution to #96 and seeing if we can just power through and
# brute force the puzzles.

BOX_OFFSETS = [0, 1, 2, 9, 10, 11, 18, 19, 20]

def get_box(idx):
    rowbox = (idx // 27)
    colbox = (idx % 9) // 3
    box_base = rowbox * 27 + colbox * 3
    return box_base

def _solve_puzzle(puzzle, idx):
    if idx >= 81:
        # We're done, we found a solution.
        return True
    elif puzzle[idx] > 0:
        # In this case, this cell is given by the puzzle, so don't
        # guess.
        return _solve_puzzle(puzzle, idx + 1)
    else:
        valid_options = {}
        for i in range(1, 10):
            valid_options[i] = True
        row = (idx // 9) * 9
        for i in range(9):
            if puzzle[row+i] > 0:
                valid_options[puzzle[row+i]] = False
        col = idx % 9
        for i in range(9):
            if puzzle[col+i*9] > 0:
                valid_options[puzzle[col+i*9]] = False
        box = get_box(idx)
        for i in BOX_OFFSETS:
            if puzzle[box+i] > 0:
                valid_options[puzzle[box+i]] = False
        for num in valid_options.keys():
            if valid_options[num]:
                # Try this solution
                puzzle[idx] = num
                if _solve_puzzle(puzzle, idx + 1):
                    return True
        # No good. Clear the cell and backtrack
        puzzle[idx] = 0
        return False

def solve_puzzle(puzzle):
    return _solve_puzzle(puzzle, 0)

total_sum = 0
with open('./files/p096_sudoku.txt') as f:
    for _ in range(50):
        f.readline() # Ignore "GRID XX" line
        puzzle = []
        for i in range(9):
            puzzle += list(map(int, f.readline()[:9]))
        assert(solve_puzzle(puzzle))
        total_sum += int(''.join(map(str, puzzle[:3])))
print(total_sum)
