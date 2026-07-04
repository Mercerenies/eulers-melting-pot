
import scala.util.control.Breaks.{breakable, break}

object problem210_2 {
  val EPSILON: Double = 0.001 // Cheap and nasty but good enough for now

  case class Point(val x: Double, val y: Double) {
    def +(that: Point) = Point(x + that.x, y + that.y)
    def -(that: Point) = Point(x - that.x, y - that.y)
    def unary_- = Point(-x, -y)

    def dot(that: Point) = x * that.x + y * that.y

    def length: Double = Math.sqrt(x * x + y * y)

    def lengthSquared: Double = x * x + y * y

    def distance(that: Point) = (that - this).length
  }

  object Point {
    def isColinear(a: Point, b: Point, c: Point) = {
      val d = (b - a).dot(c - a)
      d * d == (b - a).lengthSquared * (c - a).lengthSquared
    }
  }

  case class Triangle(val p: Point, val q: Point, val r: Point) {
    def a: Double = (q - p).length
    def b: Double = (r - q).length
    def c: Double = (p - r).length

    def sideLengths: List[Double] = List(a, b, c)

    def α: Double =
      Math.acos((b * b + c * c - a * a) / (2 * b * c))

    def β: Double =
      Math.acos((c * c + a * a - b * b) / (2 * c * a))

    def γ: Double =
      Math.acos((a * a + b * b - c * c) / (2 * a * b))

    def angles: List[Double] = List(α, β, γ)

    def isDegenerate: Boolean = sideLengths.exists(_ == 0) || Point.isColinear(p, q, r)

    def isObtuse: Boolean = angles.exists(_ > Math.PI / 2 + EPSILON)

    def anglesIsObtuse: List[Boolean] = angles.map(_ > Math.PI / 2 + EPSILON)
  }

  extension(self: Boolean)
    def toLong: Long =
      if self then 1 else 0

  extension(a: Long)
    def ceilDiv(b: Long): Long =
      (a + b - 1) / b

  def point_o = Point(0, 0)
  def point_c(r: Long) = Point(r.toDouble / 4, r.toDouble / 4)

  def s(r: Long): Seq[Point] =
    for {
      x <- (- r) to r
      ylim = r - x.abs
      y <- (- ylim) to ylim
    } yield Point(x.toDouble, y.toDouble)

  def triangle(r: Long, b: Point): Triangle =
    Triangle(point_o, point_c(r), b)

  def triangular(k: Long): Long =
    k * (k + 1) / 2

  def integerPointsInRectangle(a: Long, b: Long): Long =
    2 * a * b - a - b + 1

  // Region (*): Annoying and difficult
  def region1(r: Long): Long = {
    val h = r.ceilDiv(4)
    val internalPoints = if (h <= 2) { 0 } else { triangular(h - 2) }
    val pointsOnRightTriangle = 2 * (h - 1)

    // Now hand-count the other points
    var pointsOutsideTriangle: Long = 0
    for (x <- 1L until r) {
      if (x % 1000 == 0) {
        println(x)
      }
      var y = r / 4 + 1
      breakable {
        while (x + y <= r) {
          val t = triangle(r, Point(x.toDouble, y.toDouble))
          if (!t.isDegenerate && t.α > Math.PI / 2 + EPSILON) {
            pointsOutsideTriangle += 1
            y += 1
          } else {
            break
          }
        }
      }
    }
    // pointsOutsideTriangle only counts those with x > 0. Reflect
    // across the r = x + y line to get the other ones above y = x.
    val allPointsOutsideTriangle = 2 * pointsOutsideTriangle
    internalPoints + pointsOnRightTriangle + allPointsOutsideTriangle
  }

  // Region (**): Easy, just a rectangle
  def region2(r: Long): Long = {
    val a = r.ceilDiv(4)
    val b = r.ceilDiv(2)
    integerPointsInRectangle(a, b) + a + b - 1
  }

  // Region (***): Easy, just a square
  def region3(r: Long): Long = {
    val a = r.ceilDiv(2)
    integerPointsInRectangle(a, a) + 2 * a - 1
  }

  def n(r: Long): Long =
    2 * (region1(r) + region2(r) + region3(r))

  def nByAngle(r: Long): List[Long] =
    List(region1(r), region2(r), region3(r))

  val UPPER = 100

  @main def main() = {
    //for (r <- (4 to UPPER).by(4)) {
    //  println(s"n($r) = [${nByAngle(r)}] ${n(r)}")
    //}
    //val theBigOne = 1_000_000_000L
    //rintln(n(theBigOne))
  }
}
