
// Optimized version of problem199.swift, which does smarter things to
// reduce redundant iterations.
//
// Performance is comparable, so either approach with work. Both this
// and problem199.swift run in about 1.3 seconds.

struct Circle: Equatable, Hashable {
    var radius: Double

    var area: Double {
        get {
            return Double.pi * radius * radius
        }
    }

    var curvature: Double {
        get {
            1 / radius
        }
    }
}

struct Gap: Equatable, Hashable {
    let a: Circle
    let b: Circle
    let c: Circle

    init(_ a: Circle, _ b: Circle, _ c: Circle) {
        // Sort the three circles by radius, for the sake of coherent
        // comparisons.
        let sorted = [a, b, c].sorted { $0.radius < $1.radius }
        self.a = sorted[0]
        self.b = sorted[1]
        self.c = sorted[2]
    }
}

class Counter<Key>: Sequence where Key: Hashable {
    private var _counts: [Key: Int] = [:]

    typealias Iterator = AnyIterator<(key: Key, value: Int)>
    typealias Element = (key: Key, value: Int)

    subscript(key: Key) -> Int {
        get {
            self._counts[key, default: 0]
        }
        set(newValue) {
            self._counts[key] = newValue
        }
    }

    func makeIterator() -> AnyIterator<(key: Key, value: Int)> {
        AnyIterator(self._counts.makeIterator())
    }
}

// Descartes' Theorem
func fourthCircle(_ gap: Gap) -> Circle {
    let k1 = gap.a.curvature
    let k2 = gap.b.curvature
    let k3 = gap.c.curvature
    let k4 = k1 + k2 + k3 + 2 * (k1 * k2 + k2 * k3 + k3 * k1).squareRoot()
    return Circle(radius: 1 / k4)
}

func iterate(gaps: Counter<Gap>, areaCovered: inout Double) -> Counter<Gap> {
  let newGaps = Counter<Gap>()
  for elem in gaps {
      let newCircle = fourthCircle(elem.key)
      areaCovered += newCircle.area * Double(elem.value)
      newGaps[Gap(newCircle, elem.key.a, elem.key.b)] += elem.value
      newGaps[Gap(newCircle, elem.key.a, elem.key.c)] += elem.value
      newGaps[Gap(newCircle, elem.key.b, elem.key.c)] += elem.value
  }
  return newGaps
}

let totalArea = Circle(radius: 1.0).area
let outerCircle = Circle(radius: -1.0) // Negative curvature
let initialInnerCircle = Circle(radius: 2.0 * 3.0.squareRoot() - 3.0)
let centerGap = Gap(initialInnerCircle, initialInnerCircle, initialInnerCircle)
let outerGap = Gap(outerCircle, initialInnerCircle, initialInnerCircle)

var gaps = Counter<Gap>()
gaps[outerGap] = 3
gaps[centerGap] = 1

var areaCovered = initialInnerCircle.area * 3
for _ in 0..<10 {
    gaps = iterate(gaps: gaps, areaCovered: &areaCovered)
}
let areaFraction = (totalArea - areaCovered) / totalArea
print((areaFraction * 1e8).rounded() / 1e8)
