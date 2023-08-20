
// Direct geometric model of the problem space. Horribly inefficient,
// I'm hoping to get up to maybe T(4) of T(5) here, to start looking
// for patterns.
//
// This program doesn't work. I'm guessing it's floating-point
// imprecision (I don't like all the floats for coordinates that we
// use here). Back to the drawing board.

struct Vector: Equatable, Hashable, Comparable {
    var x: Double
    var y: Double

    static func +(left: Vector, right: Vector) -> Vector {
        Vector(x: left.x + right.x, y: left.y + right.y)
    }

    static func *(left: Vector, right: Vector) -> Vector {
        Vector(x: left.x * right.x, y: left.y * right.y)
    }

    static func *(left: Vector, right: Double) -> Vector {
        Vector(x: left.x * right, y: left.y * right)
    }

    static prefix func -(vector: Vector) -> Vector {
        Vector(x: -vector.x, y: -vector.y)
    }

    static func -(left: Vector, right: Vector) -> Vector {
        left + -right
    }

    static func <(left: Vector, right: Vector) -> Bool {
        if left.x == right.x {
            return left.y < right.y
        } else {
            return left.x < right.x
        }
    }

}

struct Line: Equatable, Hashable {
    let source: Vector
    let destination: Vector

    init(source: Vector, destination: Vector) {
        // For coherence, normalize the two points so that source <=
        // destination.
        if source <= destination {
            self.source = source
            self.destination = destination
        } else {
            self.source = destination
            self.destination = source
        }
    }

    var direction: Vector {
        destination - source
    }

    func flipped() -> Line {
        Line(source: source * Vector(x: 1, y: -1), destination: destination * Vector(x: 1, y: -1))
    }

    static func +(left: Line, right: Vector) -> Line {
        Line(source: left.source + right, destination: left.destination + right)
    }

    static func *(left: Line, right: Double) -> Line {
        Line(source: left.source * right, destination: left.destination * right)
    }

}

func isParallel(_ v1: Vector, _ v2: Vector) -> Bool {
    return v1.x * v2.y == v1.y * v2.x
}

func isParallel(_ line1: Line, _ line2: Line) -> Bool {
    isParallel(line1.direction, line2.direction)
}

func mergeLineSet(_ left: inout Set<Line>, _ right: Set<Line>) {
    var newLines: Set<Line>
    repeat {
        newLines = []
        for line1 in left {
            for line2 in right {
                if isParallel(line1, line2) {
                    if line1.destination == line2.source {
                        let combinedLine = Line(source: line1.source, destination: line2.destination)
                        if !left.contains(combinedLine) {
                            newLines.insert(combinedLine)
                        }
                    }
                    if line2.destination == line1.source {
                        let combinedLine = Line(source: line2.source, destination: line1.destination)
                        if !left.contains(combinedLine) {
                            newLines.insert(combinedLine)
                        }
                    }
                }
            }
        }
        left.formUnion(newLines)
    } while !newLines.isEmpty
    left.formUnion(right)
}

func normalizeLineSet(lines: inout Set<Line>) {
    mergeLineSet(&lines, lines)
}

class GeometricSpace {
    private var lines: Set<Line>

    init(lines: Set<Line>) {
        self.lines = lines
        normalizeLineSet(lines: &self.lines)
    }

    func copy() -> GeometricSpace {
        GeometricSpace(lines: self.lines)
    }

    func countTriangles() -> Int {
        var count = 0
        for line1 in lines {
            for line2 in lines {
                if line1.destination == line2.source && !isParallel(line1, line2) {
                    let missingLine = Line(source: line1.source, destination: line2.destination)
                    if lines.contains(missingLine) {
                        count += 1
                    }
                }
            }
        }
        return count
    }

    func transform(_ function: (Line) -> Line) {
        lines = Set(lines.map(function))
    }

    func shift(_ delta: Vector) {
        transform { $0 + delta }
    }

    func flip() {
        transform { $0.flipped() }
    }

    static func +=(left: inout GeometricSpace, right: GeometricSpace) {
        mergeLineSet(&left.lines, right.lines)
    }

    static func +(left: GeometricSpace, right: GeometricSpace) -> GeometricSpace {
        var newSpace = left.copy()
        newSpace += right
        return newSpace
    }

}

func expand(space: inout GeometricSpace, sizeMultiplier: Double) {
    let lowerLeft = space.copy()
    lowerLeft.shift(Vector(x: -0.8660254038, y: 0.5) * sizeMultiplier)
    let lowerRight = space.copy()
    lowerRight.shift(Vector(x: 0.8660254038, y: 0.5) * sizeMultiplier)
    let upper = space.copy()
    upper.shift(Vector(x: 0, y: -1) * sizeMultiplier)
    space.flip()
    space += lowerLeft
    space += lowerRight
    space += upper
}

func size1() -> GeometricSpace {
    let center = Vector(x: 0, y: 0)
    let vertices = [
      Vector(x: 0, y: -1),
      Vector(x: 0.8660254038, y: 0.5),
      Vector(x: -0.8660254038, y: 0.5),
    ]
    let midpoints = [
      Vector(x: 0, y: 0.5),
      Vector(x: -0.4330127019, y: -0.25),
      Vector(x: 0.4330127019, y: -0.25),
    ]
    return GeometricSpace(lines: [
      Line(source: center, destination: vertices[0]),
      Line(source: center, destination: vertices[1]),
      Line(source: center, destination: vertices[2]),
      Line(source: center, destination: midpoints[0]),
      Line(source: center, destination: midpoints[1]),
      Line(source: center, destination: midpoints[2]),
      Line(source: vertices[0], destination: midpoints[1]),
      Line(source: vertices[0], destination: midpoints[2]),
      Line(source: vertices[1], destination: midpoints[0]),
      Line(source: vertices[1], destination: midpoints[2]),
      Line(source: vertices[2], destination: midpoints[0]),
      Line(source: vertices[2], destination: midpoints[1]),
    ])
}

var geometry = size1()
var sizeMultiplier = 1.0
for i in 1...2 {
    print("T(\(i)) = \(geometry.countTriangles())")
    expand(space: &geometry, sizeMultiplier: sizeMultiplier)
    sizeMultiplier *= 2
}
