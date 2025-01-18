
// It's Descartes' Theorem. Descartes' Theorem tells us the radius in
// both the edge case and the internal case (the edge case is treated
// as a circle with negative curvature.
//
// https://en.wikipedia.org/wiki/Descartes%27_theorem#Statement
//
// And... no optimizations necessary, takes 1.3 seconds in Swift. I
// may try some optimizations anyway just to see how far this rabbit
// hole goes, but this works.

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

struct WithCoefficient<T> {
    let value: T
    let coefficient: Int
}

// Descartes' Theorem
func fourthCircle(_ gap: Gap) -> Circle {
    let k1 = gap.a.curvature
    let k2 = gap.b.curvature
    let k3 = gap.c.curvature
    let k4 = k1 + k2 + k3 + 2 * (k1 * k2 + k2 * k3 + k3 * k1).squareRoot()
    return Circle(radius: 1 / k4)
}

func iterate(gaps: [WithCoefficient<Gap>], areaCovered: inout Double) -> [WithCoefficient<Gap>] {
  var newGaps: [WithCoefficient<Gap>] = []
  for gap in gaps {
      let newCircle = fourthCircle(gap.value)
      areaCovered += newCircle.area * Double(gap.coefficient)
      newGaps.append(WithCoefficient(value: Gap(newCircle, gap.value.a, gap.value.b), coefficient: gap.coefficient))
      newGaps.append(WithCoefficient(value: Gap(newCircle, gap.value.b, gap.value.c), coefficient: gap.coefficient))
      newGaps.append(WithCoefficient(value: Gap(newCircle, gap.value.a, gap.value.c), coefficient: gap.coefficient))
  }
  return newGaps
}

let totalArea = Circle(radius: 1.0).area
let outerCircle = Circle(radius: -1.0) // Negative curvature
let initialInnerCircle = Circle(radius: 2.0 * 3.0.squareRoot() - 3.0)
let centerGap = Gap(initialInnerCircle, initialInnerCircle, initialInnerCircle)
let outerGap = Gap(outerCircle, initialInnerCircle, initialInnerCircle)

var gaps = [WithCoefficient(value: outerGap, coefficient: 3), WithCoefficient(value: centerGap, coefficient: 1)]
var areaCovered = initialInnerCircle.area * 3
for _ in 0..<10 {
    gaps = iterate(gaps: gaps, areaCovered: &areaCovered)
}
let areaFraction = (totalArea - areaCovered) / totalArea
print((areaFraction * 1e8).rounded() / 1e8)
