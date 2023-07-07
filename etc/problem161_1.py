
# Knuth's Algorithm X with dancing links. Correct algorithm, works for
# 2x9 but not remotely for 9x12.
#
# By the way, it's https://oeis.org/A215827

from __future__ import annotations

from dataclasses import dataclass
from typing import Sequence, Iterable, Iterator, cast
from pprint import pprint
from abc import abstractmethod, ABC


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


class Node:
    is_header: bool
    point: Point
    left: Node
    right: Node
    up: Node
    down: Node

    def __init__(self, point: Point, is_header: bool = False):
        self.is_header = is_header
        self.point = point
        self.left = None  # type: ignore
        self.right = None  # type: ignore
        self.up = None  # type: ignore
        self.down = None  # type: ignore

    def __repr__(self) -> str:
        return f"<Node is_header={self.is_header} point={self.point}>"

    def link_right(self, next_node: Node) -> None:
        self.right = next_node
        next_node.left = self

    def link_down(self, next_node: Node) -> None:
        self.down = next_node
        next_node.up = self

    def remove_self(self):
        self.left.right = self.right
        self.right.left = self.left
        self.up.down = self.down
        self.down.up = self.up

    def reinsert_self(self):
        self.left.right = self
        self.right.left = self
        self.up.down = self
        self.down.up = self


class DancingLinksGrid:
    """A grid of the ones in the matrix. Additionally, we have
    a header row that consists of all of the column headers, plus an
    extra one in the top-left. The top-left one is the "control node",
    whihc always exists and serves as ground truth.

    """
    control_node: Node
    grid: list[list[Node]]

    def __init__(self, width: int, height: int) -> None:
        self.grid = [[Node(Point(x, y - 1), is_header=(y == 0)) for x in range(width)] for y in range(height + 1)]  # noqa: E501
        self.control_node = Node(Point(99999, 99999), is_header=True)
        for x in range(width):
            for y in range(height + 1):
                self.grid[y][x].link_right(self.grid[y][(x + 1) % width])
                self.grid[y][x].link_down(self.grid[(y + 1) % (height + 1)][x])
        # Link up control node
        self.control_node.link_down(self.control_node)
        self.control_node.link_right(self.grid[0][0])
        self.grid[0][-1].link_right(self.control_node)

    def is_empty(self) -> bool:
        return self.control_node.left == self.control_node.right == self.control_node  # noqa: E501

    def get(self, x: int, y: int) -> Node:
        return self.grid[y + 1][x]

    def get_header(self, x: int) -> Node:
        return self.grid[0][x]

    @property
    def header_row(self) -> Sequence[Node]:
        return self.grid[0]

    @property
    def width(self) -> int:
        return len(self.grid[0])

    def purge_zeroes(self, matrix: list[list[int]]) -> None:
        for y in range(len(matrix)):
            for x in range(len(matrix[y])):
                if matrix[y][x] == 0:
                    self.get(x, y).remove_self()


class Instruction(ABC):

    @abstractmethod
    def forward(self, grid: DancingLinksGrid) -> None:
        ...

    @abstractmethod
    def backward(self, grid: DancingLinksGrid) -> None:
        ...


class RemoveNode(Instruction):

    def __init__(self, x: int, y: int) -> None:
        self.x = x
        self.y = y

    def forward(self, grid: DancingLinksGrid) -> None:
        grid.get(self.x, self.y).remove_self()

    def backward(self, grid: DancingLinksGrid) -> None:
        grid.get(self.x, self.y).reinsert_self()

    def __hash__(self) -> int:
        return hash(("RemoveNode", self.x, self.y))

    def __eq__(self, other):
        return isinstance(other, RemoveNode) and (self.x, self.y) == (other.x, other.y)  # noqa: E501


class RemoveHeader(Instruction):

    def __init__(self, x: int) -> None:
        self.x = x

    def forward(self, grid: DancingLinksGrid) -> None:
        grid.get_header(self.x).remove_self()

    def backward(self, grid: DancingLinksGrid) -> None:
        grid.get_header(self.x).reinsert_self()

    def __hash__(self) -> int:
        return hash(("RemoveHeader", self.x))

    def __eq__(self, other):
        return isinstance(other, RemoveHeader) and self.x == other.x  # noqa: E501


class Seq(Instruction):

    def __init__(self, instrs: Sequence[Instruction]) -> None:
        self.instructions = instrs

    def forward(self, grid: DancingLinksGrid) -> None:
        for instr in self.instructions:
            instr.forward(grid)

    def backward(self, grid: DancingLinksGrid) -> None:
        for instr in reversed(self.instructions):
            instr.backward(grid)


TRIOMINOES = [
    Triomino(Point(0, 0), Point(0, 1), Point(1, 0)),  # L (Flipped)
    Triomino(Point(0, 0), Point(1, 1), Point(1, 0)),  # L (Rotated 180)
    Triomino(Point(0, 0), Point(0, 1), Point(1, 1)),  # L
    Triomino(Point(1, 0), Point(0, 1), Point(1, 1)),  # L (Mirrored)
    Triomino(Point(0, 0), Point(1, 0), Point(2, 0)),  # I (Rotated 90)
    Triomino(Point(0, 0), Point(0, 1), Point(0, 2)),  # I
]


def build_coordinate_grid(width: int, height: int) -> dict[Point, int]:
    coordinates = {}
    coordinate_index = 0
    for y in range(height):
        for x in range(width):
            coordinates[Point(x, y)] = coordinate_index
            coordinate_index += 1
    return coordinates


def construct_row(coordinates: dict[Point, int],
                  source: Point,
                  triomino: Triomino) -> list[int] | None:
    row = [0] * len(coordinates)
    # Identify and fill in all positions. If we ever encounter a
    # position where there is no index for its coordinate, it's out of
    # bounds, which means this is not a valid placement for a
    # triomino. So return None.
    for relative_position in triomino:
        coordinate_index = coordinates.get(source + relative_position)
        if coordinate_index is None:
            return None
        row[coordinate_index] = 1
    return row


def build_matrix(width: int, height: int) -> list[list[int]]:
    coordinates = build_coordinate_grid(width, height)
    result = []
    for y in range(height):
        for x in range(width):
            for triomino in TRIOMINOES:
                row = construct_row(coordinates, Point(x, y), triomino)
                if row is not None:
                    result.append(row)
    return result


def count_ones(header_node: Node) -> int:
    count = 0
    node = header_node.down
    while header_node != node:
        count += 1
        node = cast(Node, node).down
    return count


def remove_row(node: Node) -> list[Instruction]:
    if node.is_header:
        return []
    instrs: list[Instruction] = []
    tmp = node.right
    while tmp != node:
        instrs.append(RemoveNode(tmp.point.x, tmp.point.y))
        tmp = tmp.right
    return instrs


def remove_column(node: Node) -> list[Instruction]:
    instrs: list[Instruction] = []
    tmp = node.down
    while tmp != node:
        if tmp.is_header:
            instrs.append(RemoveHeader(tmp.point.x))
        else:
            instrs.append(RemoveNode(tmp.point.x, tmp.point.y))
        tmp = tmp.down
    return instrs


def count_columns(grid):
    node = grid.control_node.right
    count = 0
    while node != grid.control_node:
        count += 1
        node = node.right
    # print("count", count)


def run_algorithm_x(grid: DancingLinksGrid) -> int:
    total = 0

    def recurse() -> None:
        nonlocal total
        count_columns(grid)
        if grid.is_empty():
            # We found a solution! Huzzah! Count it and return.
            total += 1
            return
        # Find the column with the minimum number of ones.
        min_column_index = 0
        min_column_best = 999999999
        node = grid.control_node.right
        # print("seek")
        while node != grid.control_node:
            # print("candidate", node.point.x)
            ones = count_ones(node)
            if ones < min_column_best:
                min_column_best = ones
                min_column_index = node.point.x
            node = node.right
        # Go through each row and do the algorithm.
        # print("column", min_column_index)
        node = grid.header_row[min_column_index].down
        while not node.is_header:
            instructions = get_instructions_for_row(node)
            instructions.forward(grid)
            recurse()
            instructions.backward(grid)
            node = node.down

    def get_instructions_for_row(source_node: Node) -> Instruction:
        # print("chosen row", source_node.point.y)
        instructions = set()
        j = source_node
        while True:
            i = j
            while True:
                # print("Removing row", i.point.y)
                instructions.update(remove_row(i))
                i = i.down
                if i == j:
                    break
            # print("Removing column", j.point.x)
            instructions.update(remove_column(j))
            j = j.right
            if j == source_node:
                break
        return Seq(list(instructions))

    recurse()
    return total


WIDTH = 6
HEIGHT = 6
matrix = build_matrix(WIDTH, HEIGHT)
# pprint(matrix)

matrix_width = len(matrix[0])
matrix_height = len(matrix)
grid = DancingLinksGrid(matrix_width, matrix_height)
grid.purge_zeroes(matrix)
result = run_algorithm_x(grid)
print(result)
