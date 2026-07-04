
// It takes a honking 22 minutes in *Scala* but it works... somehow.
// Regions (**) and (***) were easy (see the diagram at
// 210_diagram.png), but I was miscounting Region (*). It's not a
// right triangle like I initially thought. Based on reading, it
// appears to be a circle, but I need to figure out why that is. At
// minimum, this version of the program brute-forces it, which is
// really slow (esp because I have to use BigInt).

import scala.util.control.Breaks.{breakable, break}
import scala.math.BigInt

object problem210_2 {
  case class Point(val x: BigInt, val y: BigInt) {
    def +(that: Point) = Point(x + that.x, y + that.y)
    def -(that: Point) = Point(x - that.x, y - that.y)
    def unary_- = Point(-x, -y)

    def dot(that: Point) = x * that.x + y * that.y

    def lengthSquared: BigInt = x * x + y * y

    //def distance(that: Point) = (that - this).length
  }

  object Point {
    def isColinear(a: Point, b: Point, c: Point) = {
      val d = (b - a).dot(c - a)
      d * d == (b - a).lengthSquared * (c - a).lengthSquared
    }
  }

  case class Triangle(val p: Point, val q: Point, val r: Point) {
    //def a: Double = (q - p).length
    //def b: Double = (r - q).length
    //def c: Double = (p - r).length

    def a2 = (q - p).lengthSquared
    def b2 = (r - q).lengthSquared
    def c2 = (p - r).lengthSquared

    //def sideLengths: List[Double] = List(a, b, c)

    //def α: Double =
    //  Math.acos((b * b + c * c - a * a) / (2 * b * c))

    //def β: Double =
    //  Math.acos((c * c + a * a - b * b) / (2 * c * a))

    //def γ: Double =
    //  Math.acos((a * a + b * b - c * c) / (2 * a * b))

    //def angles: List[Double] = List(α, β, γ)

    def isDegenerate: Boolean = List(a2, b2, c2).exists(_ == 0) || Point.isColinear(p, q, r)

    //def isObtuse: Boolean = angles.exists(_ > Math.PI / 2 + EPSILON)

    //def anglesIsObtuse: List[Boolean] = angles.map(_ > Math.PI / 2 + EPSILON)

    def alphaIsObtuse: Boolean = {
      // Okay, so we want to check whether α > 90°, or eqv α > π / 2.
      // α is defined as acos(stuff), so we want to check whether
      // acos(stuff) > π / 2. Equivalently, we want to check that
      // 'stuff' < 0. The denominator of 'stuff' is 2 * b * c, a
      // positive quantity, so we equivalently want to check that its
      // numerator is < 0, i.e. b² + c² - a² < 0. Good news! No need
      // for square roots anymore!
      val num = (r - q).lengthSquared + (p - r).lengthSquared - (q - p).lengthSquared
      num < 0
    }
  }

  extension(a: BigInt)
    def ceilDiv(b: BigInt): BigInt =
      (a + b - 1) / b

  def point_o = Point(0, 0)
  def point_c(r: BigInt) = Point(r / 4, r / 4)

  def triangle(r: BigInt, b: Point): Triangle =
    Triangle(point_o, point_c(r), b)

  def triangular(k: BigInt): BigInt =
    k * (k + 1) / 2

  def integerPointsInRectangle(a: BigInt, b: BigInt): BigInt =
    2 * a * b - a - b + 1

  // Given a sequence for whom a prefix satisfies p and the rest do
  // not, returns the index of the first element that does not.
  def binarySearch[A](elems: IndexedSeq[A])(p: A => Boolean): Int = {
    var lower = 0
    var higher = elems.size
    while (lower < higher) {
      val pivot = (lower + higher) / 2
      if (p(elems(pivot))) {
        lower = pivot + 1
      } else {
        higher = pivot
      }
    }
    lower
  }

  // Region (*): Annoying and difficult
  def region1(r: BigInt): BigInt = {
    val h = r.ceilDiv(4)
    val internalPoints = if (h <= 2) { BigInt(0) } else { triangular(h - 2) }
    val pointsOnRightTriangle = 2 * (h - 1)

    // Now hand-count the other points
    var pointsOutsideTriangle: BigInt = 0
    for (x <- BigInt(1) until (r / 4)) {
      if (x % 1_000_000 == 0) {
        println(x)
      }
      val ys = (r / 4 + 1) to (r - x)
      val ysWithObtuse = binarySearch(ys) { y =>
        val t = triangle(r, Point(x, y))
        !t.isDegenerate && t.alphaIsObtuse
      }
      pointsOutsideTriangle += ysWithObtuse
    }
    // pointsOutsideTriangle only counts those with x > 0. Reflect
    // across the r = x + y line to get the other ones above y = x.
    val allPointsOutsideTriangle = 2 * pointsOutsideTriangle
    internalPoints + pointsOnRightTriangle + allPointsOutsideTriangle
  }

  // Region (**): Easy, just a rectangle
  def region2(r: BigInt): BigInt = {
    val a = r.ceilDiv(4)
    val b = r.ceilDiv(2)
    integerPointsInRectangle(a, b) + a + b - 1
  }

  // Region (***): Easy, just a square
  def region3(r: BigInt): BigInt = {
    val a = r.ceilDiv(2)
    integerPointsInRectangle(a, a) + 2 * a - 1
  }

  def n(r: BigInt): BigInt =
    2 * (region1(r) + region2(r) + region3(r))

  def nByAngle(r: BigInt): List[BigInt] =
    List(region1(r), region2(r), region3(r))

  val UPPER = 100

  @main def main() = {
    // Test binary search
    //val seq = 1 to 10_000
    //val index = binarySearch(seq) { _ < 100 }
    //println(index)

    for (r <- (4 to UPPER).by(4)) {
      println(s"n($r) = [${nByAngle(r)}] ${n(r)}")
    }
    val theBigOne = 1_000_000_000L
    println(n(theBigOne))
  }
}
