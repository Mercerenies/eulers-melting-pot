
// Brute force for the small inputs

object problem210_1 {
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
  }

  def point_o = Point(0, 0)
  def point_c(r: Int) = Point(r.toDouble / 4, r.toDouble / 4)

  def s(r: Int): Seq[Point] =
    for {
      x <- (- r) to r
      ylim = r - x.abs
      y <- (- ylim) to ylim
    } yield Point(x, y)

  def n(r: Int): Int =
    s(r).view
      .map { b => Triangle(point_o, point_c(r), b) }
      .count { t => !t.isDegenerate && t.isObtuse }

  @main def main() = {
    for (r <- 1 to 100) {
      println(s"n($r) = " + n(r))
    }
  }
}
