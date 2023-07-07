
# Let's do recurrence relations now.

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Iterator, Sequence, TypeVar, cast
from collections import deque
from pprint import pprint


_T = TypeVar("_T")


@dataclass(frozen=True)
class Point:
    x: int
    y: int

    def __add__(self, other: Point) -> Point:
        return Point(self.x + other.x, self.y + other.y)


class Triomino(Iterable[Point]):
    points: Sequence[Point]

    def __init__(self, *points: Point):
        self.points = points

    def __iter__(self) -> Iterator[Point]:
        return iter(self.points)

    def __len__(self) -> int:
        return len(self.points)

    def __add__(self, point: Point) -> Triomino:
        return Triomino(*map(lambda x: x + point, self.points))


TRIOMINOES = [
    Triomino(Point(0, 0), Point(0, 1), Point(1, 0)),  # L (Flipped)
    Triomino(Point(0, 0), Point(1, 1), Point(1, 0)),  # L (Rotated 180)
    Triomino(Point(0, 0), Point(0, 1), Point(1, 1)),  # L
    Triomino(Point(0, 0), Point(-1, 1), Point(0, 1)),  # L (Mirrored)
    Triomino(Point(0, 0), Point(1, 0), Point(2, 0)),  # I (Rotated 90)
    Triomino(Point(0, 0), Point(0, 1), Point(0, 2)),  # I
]


class Grid:
    impl: tuple[tuple[bool, ...], ...]

    def __init__(self, impl: tuple[tuple[bool, ...], ...]) -> None:
        self.impl = impl

    @classmethod
    def empty(self, width: int, height: int) -> Grid:
        return Grid((((False,) * width),) * height)

    def __repr__(self) -> str:
        return f"Grid({self.impl!r})"

    def __eq__(self, other) -> bool:
        return isinstance(other, Grid) and self.impl == other.impl

    def __hash__(self) -> int:
        return hash(("Grid", self.impl))

    def origin(self) -> Point:
        for y, row in enumerate(self.impl):
            for x, cell in enumerate(row):
                if not cell:
                    return Point(x, y)
        raise RuntimeError("Grid is all full")

    def remove_full_rows(self) -> tuple[Grid, int]:
        new_grid = self.impl
        width = len(self.impl[0])
        rows_removed = 0
        while all(new_grid[0]):  # While the top row is full
            new_grid = new_grid[1:] + ((False,) * width,)
            rows_removed += 1
        return Grid(new_grid), rows_removed

    def fill_position(self, position: Point) -> Grid | None:
        # If the position is already filled or is out of bounds, we
        # can't fill it, so return None.
        try:
            if position.x < 0 or position.y < 0 or self.impl[position.y][position.x]:  # noqa: E501
                return None
        except IndexError:
            return None
        return Grid(_replace_in_tuple(self.impl, position.y, position.x))

    def place(self, triomino: Triomino) -> Grid | None:
        grid: Grid | None = self
        for pos in triomino:
            grid = cast(Grid, grid).fill_position(pos)
            if grid is None:
                return None
        return grid


@dataclass(frozen=True)
class Term:
    state: int
    relative_row: int

    def __str__(self) -> str:
        return f"({self.state}, {self.relative_row})"

    def eval(self, table: Sequence[Sequence[int]], y: int) -> int:
        if 0 <= y + self.relative_row < len(table):
            return table[y + self.relative_row][self.state]
        return 0  # Invalid state; automatically zero


class Equation:
    terms: tuple[Term, ...]

    def __init__(self, *terms: Term) -> None:
        self.terms = terms

    def __str__(self) -> str:
        return ' + '.join(map(str, self.terms))

    def __repr__(self) -> str:
        return f"Equation({','.join(map(repr, self.terms))})"

    def __add__(self, term: Term) -> Equation:
        return Equation(*self.terms + (term,))

    def eval(self, table: Sequence[Sequence[int]], y: int) -> int:
        total = 0
        for term in self.terms:
            total += term.eval(table, y)
        return total


# For triominoes, 3 is always the maximum amount we need to store.
HEIGHT = 3


def build_state_table(width: int) -> dict[Grid, int]:
    result = {}
    current_index = 0
    frontier = deque([Grid.empty(width, HEIGHT)])
    visited = set()
    while frontier:
        current = frontier.pop()
        if current in visited:
            continue
        visited.add(current)
        result[current] = current_index
        current_index += 1
        origin = current.origin()
        for triomino in TRIOMINOES:
            new_grid = current.place(triomino + origin)
            if new_grid is not None:
                normalized_grid, _ = new_grid.remove_full_rows()
                frontier.appendleft(normalized_grid)
    return result


def build_equation(states: dict[Grid, int], grid: Grid) -> Equation:
    equation = Equation()
    origin = grid.origin()
    for triomino in TRIOMINOES:
        new_grid = grid.place(triomino + origin)
        if new_grid is not None:
            normalized_grid, rows_removed = new_grid.remove_full_rows()
            equation += Term(states[normalized_grid], - rows_removed)
    return equation


def build_equation_table(states: dict[Grid, int]) -> dict[int, Equation]:
    result = {}
    for grid, state in states.items():
        result[state] = build_equation(states, grid)
    return result


def _replace(tup: tuple[_T, ...], new_index: int, new_value: _T) -> tuple[_T, ...]:  # noqa: E501
    lst = list(tup)
    lst[new_index] = new_value
    return tuple(lst)


def _replace_in_tuple(tup: tuple[tuple[bool, ...], ...], y: int, x: int) -> tuple[tuple[bool, ...], ...]:  # noqa: E501
    new_row = _replace(tup[y], x, True)
    return _replace(tup, y, new_row)


def produce_dependency_graph(states: dict[Grid, int]) -> dict[int, list[int]]:
    result: dict[int, list[int]] = {k: [] for k in states.values()}
    for grid, index in states.items():
        origin = grid.origin()
        for triomino in TRIOMINOES:
            new_grid = grid.place(triomino + origin)
            if new_grid is not None:
                normalized_grid, count = new_grid.remove_full_rows()
                # If the count is not zero, then this is using data
                # from a previous row, so it doesn't add a dependency
                # in the current row.
                if count == 0:
                    result[index].append(states[normalized_grid])
    return result


def topological_sort(graph: dict[_T, list[_T]]) -> list[_T]:
    result = []
    used = set()

    # Incoming map
    incoming_map: dict[_T, list[_T]] = {k: [] for k in graph.keys()}
    for k, vs in graph.items():
        for v in vs:
            incoming_map[v].append(k)

    # Calculate nodes with none incoming
    no_incoming = set(key for key, value in incoming_map.items() if not value)

    while no_incoming:
        current = next(iter(no_incoming))
        no_incoming.remove(current)
        result.append(current)
        used.add(current)
        for m in graph[current]:
            if m not in used and all(v in used for v in incoming_map[m]):
                no_incoming.add(m)

    if len(result) < len(graph):
        raise RuntimeError("No topological order exists")
    return result


WIDTH = 9
GOAL = 12
states = build_state_table(WIDTH)
equations = build_equation_table(states)
values_table = [[-9999] * len(states) for _ in range(GOAL + 1)]
values_table[0] = [0] * len(states)

# Initial state is our "fundamental" solution.
values_table[0][0] = 1

# Now we need to identify an order in which to compute the cells, to
# make sure that all dependencies are taken into consideration. Form a
# dependency graph and then find a topological ordering of it.
dependency_graph = produce_dependency_graph(states)
order = topological_sort(dependency_graph)
# We want to evaluate dependencies before the things they depend on,
# so reverse it.
order.reverse()

for y in range(1, GOAL + 1):
    for x in order:
        values_table[y][x] = equations[x].eval(values_table, y)

print(values_table[-1][0])
