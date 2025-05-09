
from enum import Enum
from functools import cache


class Solutions(Enum):
    ZERO = 0
    ONE = 1
    MANY = 2


def max_sum_squares(max_index: int, count: int) -> int:
    n = max_index
    m = max_index - count
    return (n * (n + 1) * (2 * n + 1) + m * (m + 1) * (2 * m + 1)) // 6


@cache
def count_ways_to_sum(value: int, addends: int, index_limit: int) -> Solutions:
    # print(f"count_ways_to_sum({value}, {addends}, {index_limit})")

    if value < 0:
        return Solutions.ZERO

    if index_limit < addends:
        return Solutions.ZERO

    if addends == 0:
        return Solutions.ONE if value == 0 else Solutions.ZERO

    if max_sum_squares(index_limit - 1, addends) < value:
        return Solutions.ZERO

    found_one = False
    for i in range(index_limit - 1, max(0, addends - 2), -1):
        subsolutions = count_ways_to_sum(value - i * i, addends - 1, i)
        # print(f"value={value}; count_ways_to_sum({value - i * i}, {addends - 1}, {i}) = {subsolutions}")
        if subsolutions is Solutions.ONE:
            if found_one:
                return Solutions.MANY
            found_one = True
        elif subsolutions is Solutions.MANY:
            return Solutions.MANY

    return Solutions.ONE if found_one else Solutions.ZERO


def unique_sums(addends: int, max_index: int) -> int:
    limit = addends * max_index ** 2
    return sum(i for i in range(0, limit + 1) if count_ways_to_sum(i, addends, max_index+1) is Solutions.ONE)  # noqa: E501


if __name__ == "__main__":
    print(unique_sums(10, 20))
