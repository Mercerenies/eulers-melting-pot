
// Rather than trying to represent the problem as a bunch of triangles
// with affine transforms applied, we represent the "types" of lines
// present in the diagram.

object Problem163 {

  data class Point(
    val x: Double,
    val y: Double,
  ) {

    operator fun plus(that: Point): Point =
      Point(this.x + that.x, this.y + that.y)

  }

  data class Line(
    val slope: Double?, // slope of null = a vertical line with infinite slope
    val origin: Point,
  ) {

    fun shifted(amount: Point): Line =
      copy(origin = origin + amount)

    fun isParallelTo(that: Line): Boolean =
      this.slope == that.slope

    // Precondition: this.isParallelTo(that) is false.
    private fun findIntersectionX(that: Line): Double {
      if (this.slope == null) {
        return this.origin.x
      }
      if (that.slope == null) {
        return that.origin.x
      }
      return ((this.slope * this.origin.x - that.slope * that.origin.x) + (that.origin.y - this.origin.y)) / (this.slope - that.slope)
    }

    // Precondition: this.isParallelTo(that) is false.
    fun findIntersection(that: Line): Point {
      if (this.slope == null) {
        assert(that.slope != null) // Prevent infinite loops if the precondition fails.
        return that.findIntersection(this)
      }
      val x = this.findIntersectionX(that)
      val y = this.slope * (x - this.origin.x) + this.origin.y
      return Point(x, y)
    }

  }

  // Produce a list of all of the lines in the triangle.
  //
  // Convention: The length of any side of the size 1 triangle is 1.
  // The side length of a triangle is always equal to our size
  // argument. The origin is the highlighted center point of the
  // triangle, where the original altitudes intersect.
  fun generateTriangle(): List<Line> {
    // The altitudes of the triangle.
    val altitudes = listOf(
      Line(null, Point(0.0, 0.0)),
      Line( 0.577350269189, Point(0.0, 0.0)), // slope =   1 / sqrt(3)
      Line(-0.577350269189, Point(0.0, 0.0)), // slope = - 1 / sqrt(3)
    )
    // The boundaries of the triangle.
    val boundaries = listOf(
      Line(0.0, Point(0.0, -0.288675134595)), // Y intercept = 1 / (2 * sqrt(3))
      Line(-1.73205080757, Point(0.25, 0.144337567297)), // slope = - sqrt(3), Y coordinate = 1 / (4 * sqrt(3))
      Line( 1.73205080757, Point(-0.25, 0.144337567297)), // slope =   sqrt(3), Y coordinate = 1 / (4 * sqrt(3))
    )
    // Put it all together.
    return altitudes
  }

}
