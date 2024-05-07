
// Naive solution to 184, just brute-forces the triangles and counts
// them.
//
// Good work, Project Euler! You made an integer sequence that
// apparently isn't in OEIS. That's fun! :)
//
// Too slow and too large, goes up to about 14 before running out of
// heap space in Java. I figured it would be, but we confirmed no
// OEIS.

import java.util.ArrayList;

public class problem184 {

  public record Vector(int x, int y) {

    public static final Vector ZERO = new Vector(0, 0);

    public int crossProductZ(Vector that) {
      return this.x * that.y - this.y * that.x;
    }

    public Vector plus(Vector that) {
      return new Vector(this.x + that.x, this.y + that.y);
    }

    public Vector minus(Vector that) {
      return new Vector(this.x - that.x, this.y - that.y);
    }

    public int magnitudeSquared() {
      return this.x * this.x + this.y * this.y;
    }

  }

  public record Triangle(Vector a, Vector b, Vector c) {

    public boolean contains(Vector p) {
      Vector ab = b.minus(a);
      Vector bc = c.minus(b);
      Vector ca = a.minus(c);
      Vector ap = p.minus(a);
      Vector bp = p.minus(b);
      Vector cp = p.minus(c);

      int s1 = Integer.signum(ab.crossProductZ(ap));
      int s2 = Integer.signum(bc.crossProductZ(bp));
      int s3 = Integer.signum(ca.crossProductZ(cp));
      // Note: If any of the numbers are zero, then we're on the
      // boundary of the triangle, which doesn't count for the
      // purposes of this challenge.
      return s1 == s2 && s2 == s3 && s3 != 0;
    }

  }

  public static ArrayList<Vector> getPoints(int radius) {
    ArrayList<Vector> points = new ArrayList<>();
    for (int x = - radius; x <= radius; x++) {
      for (int y = - radius; y <= radius; y++) {
        if (x * x + y * y < radius * radius) {
          points.add(new Vector(x, y));
        }
      }
    }
    return points;
  }

  public static ArrayList<Triangle> getTriangles(ArrayList<Vector> points) {
    // Note: Technically, this will generate "improper" triangles
    // which consist of three co-linear points. I don't care, because
    // those triangles have no interior and will never contain the
    // origin.
    ArrayList<Triangle> triangles = new ArrayList<>();
    for (int i = 0; i < points.size(); i++) {
      for (int j = i + 1; j < points.size(); j++) {
        for (int k = j + 1; k < points.size(); k++) {
          Triangle t = new Triangle(points.get(i), points.get(j), points.get(k));
          triangles.add(t);
        }
      }
    }
    return triangles;
  }

  public static long countI(int radius) {
    var points = getPoints(radius);
    var triangles = getTriangles(points);
    return triangles.stream().filter((x) -> x.contains(Vector.ZERO)).count();
  }

  public static void main(String[] args) {
    for (int r = 1; r < 10; r++) {
      System.out.println(r + " " + countI(r));
    }
  }

}
