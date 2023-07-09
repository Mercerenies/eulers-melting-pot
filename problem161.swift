
struct Point: Equatable, Hashable {
    var x: Int
    var y: Int

    static func +(left: Point, right: Point) -> Point {
        Point(x: left.x + right.x, y: left.y + right.y)
    }

}

struct Triomino: Sequence {
    var points: [Point]

    init(_ points: [Point]) {
        self.points = points
    }

    init(_ points: Point...) {
        self.init(points)
    }

    typealias Iterator = Array<Point>.Iterator

    func makeIterator() -> Array<Point>.Iterator {
        points.makeIterator()
    }

    static func +(left: Triomino, right: Point) -> Triomino {
        Triomino(left.points.map { $0 + right })
    }

}

let TRIOMINOES = [
  Triomino(Point(x: 0, y: 0), Point(x:  0, y: 1), Point(x: 1, y: 0)),  // L (Flipped)
  Triomino(Point(x: 0, y: 0), Point(x:  1, y: 1), Point(x: 1, y: 0)),  // L (Rotated 180)
  Triomino(Point(x: 0, y: 0), Point(x:  0, y: 1), Point(x: 1, y: 1)),  // L
  Triomino(Point(x: 0, y: 0), Point(x: -1, y: 1), Point(x: 0, y: 1)),  // L (Mirrored)
  Triomino(Point(x: 0, y: 0), Point(x:  1, y: 0), Point(x: 2, y: 0)),  // I (Rotated 90)
  Triomino(Point(x: 0, y: 0), Point(x:  0, y: 1), Point(x: 0, y: 2)),  // I
]

struct Grid: Equatable, Hashable {
    private let impl: [[Bool]]

    init(_ impl: [[Bool]]) {
        self.impl = impl
    }

    init(width: Int, height: Int) {
        let row = Array(repeating: false, count: width)
        self.init(Array(repeating: row, count: height))
    }

    func origin() -> Point {
        for (y, row) in impl.enumerated() {
            for (x, cell) in row.enumerated() {
                if !cell {
                    return Point(x: x, y: y)
                }
            }
        }
        fatalError("Grid is all full")
    }

    func removeFullRows() -> (newGrid: Grid, rowsRemoved: Int) {
        var newGrid = impl
        var rowsRemoved = 0
        let width = newGrid[0].count
        while newGrid[0].allSatisfy({ $0 }) {
            newGrid = newGrid[1...] + [Array(repeating: false, count: width)]
            rowsRemoved += 1
        }
        return (Grid(newGrid), rowsRemoved)
    }

    func fillPosition(_ position: Point) -> Grid? {
        // If the position is already filled or is out of bounds, we
        // can't fill it, so return None.
        if position.x < 0 || position.y < 0 || position.x >= impl[0].count || position.y >= impl.count {
            return nil
        }
        if impl[position.y][position.x] {
            return nil
        }
        // Otherwise, fill in the position and make a new grid.
        var gridData = impl
        gridData[position.y][position.x] = true
        return Grid(gridData)
    }

    func place(_ triomino: Triomino) -> Grid? {
        var grid = self
        for position in triomino {
            let newGrid = grid.fillPosition(position)
            guard let newGrid else { return nil }
            grid = newGrid
        }
        return grid
    }

}

struct Term: CustomStringConvertible {
    var state: Int
    var relativeRow: Int

    var description: String {
        "(\(state), \(relativeRow))"
    }

    func eval(table: [[Int]], y: Int) -> Int {
        let adjustedY = y + relativeRow
        if adjustedY >= 0 && adjustedY < table.count {
            return table[adjustedY][state]
        } else {
            return 0  // Out-of-bounds state; return zero
        }
    }

}

struct Equation: CustomStringConvertible {
    var terms: [Term]

    init(terms: [Term]) {
        self.terms = terms
    }

    init(terms: Term...) {
        self.init(terms: terms)
    }

    var description: String {
        terms.map { String(describing: $0) }.joined(separator: " + ")
    }

    static func +(left: Equation, right: Term) -> Equation {
        Equation(terms: left.terms + [right])
    }

    static func +=(left: inout Equation, right: Term) {
        left = left + right
    }

    func eval(table: [[Int]], y: Int) -> Int {
        terms.map { $0.eval(table: table, y: y) }.reduce(0, +)
    }

}

// For triominoes, 3 is always the maximum height we need to store,
// regardless of the actual height of the intended shape.
let HEIGHT = 3

typealias State = Int

func buildStateTable(width: Int) -> [Grid: State] {
    var result: [Grid: Int] = [:]
    var currentIndex = 0
    var frontier = [Grid(width: width, height: HEIGHT)]
    var visited: Set<Grid> = []
    while !frontier.isEmpty {
        let current = frontier.popLast()!
        if visited.contains(current) {
            continue
        }
        visited.insert(current)
        result[current] = currentIndex
        currentIndex += 1
        let origin = current.origin()
        for triomino in TRIOMINOES {
            if let newGrid = current.place(triomino + origin) {
                let (normalizedGrid, _) = newGrid.removeFullRows()
                frontier.append(normalizedGrid)
            }
        }
    }
    return result
}

func buildEquation(states: [Grid: State], for grid: Grid) -> Equation {
    var equation = Equation()
    let origin = grid.origin()
    for triomino in TRIOMINOES {
        if let newGrid = grid.place(triomino + origin) {
            let (normalizedGrid, rowsRemoved) = newGrid.removeFullRows()
            equation += Term(state: states[normalizedGrid]!, relativeRow: -rowsRemoved)
        }
    }
    return equation
}

func buildEquationTable(states: [Grid: State]) -> [State: Equation] {
    var result: [State: Equation] = [:]
    for (grid, state) in states {
        result[state] = buildEquation(states: states, for: grid)
    }
    return result
}

func dictWithKeys<S: Sequence, V>(with keys: S, defaultValue: V) -> [S.Element: V] {
    var result: [S.Element: V] = [:]
    for key in keys {
        result[key] = defaultValue
    }
    return result
}

func produceDependencyGraph(states: [Grid: State]) -> [State: [State]] {
    // Start with a graph of no dependencies.
    var result: [State: [State]] = dictWithKeys(with: states.values, defaultValue: [])
    for (grid, index) in states {
        let origin = grid.origin()
        for triomino in TRIOMINOES {
            guard let newGrid = grid.place(triomino + origin) else { continue }
            let (normalizedGrid, count) = newGrid.removeFullRows()
            // If the count is not zero, then this is using data from
            // a previous row, so it doesn't add a dependency on the
            // current row.
            if count == 0 {
                result[index]!.append(states[normalizedGrid]!)
            }
        }
    }
    return result
}

struct NoTopologicalSort: Error {}

func topologicalSort<T: Hashable>(of graph: [T: [T]]) throws -> [T] {
    var result: [T] = []
    var used: Set<T> = []

    // Build the incoming map
    var incomingMap: [T: [T]] = dictWithKeys(with: graph.keys, defaultValue: [])
    for (k, vs) in graph {
        for v in vs {
            incomingMap[v]!.append(k)
        }
    }

    // Calculate nodes with none incoming
    var noIncoming = incomingMap.keys.filter { key in incomingMap[key]!.isEmpty }

    while !noIncoming.isEmpty {
        let current = noIncoming.popLast()!
        result.append(current)
        used.insert(current)
        for m in graph[current]! {
            if !used.contains(m) && incomingMap[m]!.allSatisfy({ v in used.contains(v) }) {
                noIncoming.append(m)
            }
        }
    }

    if result.count < graph.count {
        throw NoTopologicalSort()
    }
    return result
}

let WIDTH = 9
let GOAL = 12

let states = buildStateTable(width: WIDTH)
let equations = buildEquationTable(states: states)

// We start with extremely negative numbers here. If our topological
// sort fails for some reason and we solve in the wrong order, we'll
// get a very negative answer which makes that problem obvious.
// Effectively, we're poisoning the well of uninitialized values.
let row = Array(repeating: -9999, count: states.count)
var valuesTable = Array(repeating: row, count: GOAL + 1)
valuesTable[0] = Array(repeating: 0, count: states.count)

// Initial state is our "fundamental" solution.
valuesTable[0][0] = 1

// Now we need to identify an order in which to compute the cells, to
// make sure that all the dependencies are taken into consideration.
// Form a dependency graph and then find a topological ordering of it.
let dependencyGraph = produceDependencyGraph(states: states)
var order = try! topologicalSort(of: dependencyGraph)
// We want to evaluate dependencies before the things they depend on,
// so reverse it.
order.reverse()

for y in 1...GOAL {
    for x in order {
        valuesTable[y][x] = equations[x]!.eval(table: valuesTable, y: y)
    }
}
print(valuesTable.last![0])
