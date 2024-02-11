
// FAAAAR too slow :(

import java.util.HashSet;

public class problem177_4 {

  public static class EightPoints {
    public int a, b, c, d, e, f, g, h;

    public EightPoints(int a, int b, int c, int d, int e, int f, int g, int h) {
      this.a = a;
      this.b = b;
      this.c = c;
      this.d = d;
      this.e = e;
      this.f = f;
      this.g = g;
      this.h = h;
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof EightPoints)) {
        return false;
      }
      EightPoints that = (EightPoints)o;
      return (this.a == that.a) && (this.b == that.b) && (this.c == that.c) && (this.d == that.d) && (this.e == that.e) && (this.f == that.f) && (this.g == that.g) && (this.h == that.h);
    }

    @Override
    public int hashCode() {
      return a + 180 * (b + 180 * (c + 180 * (d + 180 * (e + 180 * (f + 180 * (g + 180 * h))))));
    }

  }

  public static class Quadrilateral {
    public HashSet<EightPoints> angles;

    public Quadrilateral(int a, int b, int c, int d, int e, int f, int g, int h) {
      angles = new HashSet<EightPoints>();
      angles.add(new EightPoints(a, b, c, d, e, f, g, h));
      angles.add(new EightPoints(c, d, e, f, g, h, a, b));
      angles.add(new EightPoints(e, f, g, h, a, b, c, d));
      angles.add(new EightPoints(g, h, a, b, c, d, e, f));
      angles.add(new EightPoints(h, g, f, e, d, c, b, a));
      angles.add(new EightPoints(b, a, h, g, f, e, d, c));
      angles.add(new EightPoints(d, c, b, a, h, g, f, e));
      angles.add(new EightPoints(f, e, d, c, b, a, h, g));
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof Quadrilateral)) {
        return false;
      }
      Quadrilateral that = (Quadrilateral)o;
      return (this.angles.equals(that.angles));
    }

    @Override
    public int hashCode() {
      return this.angles.hashCode();
    }

  }

  public static double[] sines;
  public static double[] cosines;

  public static final double EPSILON = 10e-9;

  public static double clamp(double value, double lower, double upper) {
    if (value < lower) {
      return lower;
    }
    if (value > upper) {
      return upper;
    }
    return value;
  }

  public static boolean isInteger(double value) {
    return Math.abs(value - Math.round(value)) < EPSILON;
  }

  public static void main(String[] args) {
    // Pre-compute sines and cosines
    sines = new double[180];
    cosines = new double[180];
    for (int i = 0; i < 180; i++) {
      sines[i] = Math.sin(Math.toRadians(i));
      cosines[i] = Math.cos(Math.toRadians(i));
    }
    // Now do the computation
    HashSet<Quadrilateral> solutions = new HashSet<Quadrilateral>();
    for (int a1 = 1; a1 < 180; a1++) {
      System.out.println(a1);
      for (int a2 = 1; a2 < 180 - a1; a2++) {
        System.out.println(a1 + " " + a2);
        for (int b1 = 1; b1 < 180 - a1 - a2; b1++) {
          int b2 = 180 - b1 - a1 - a2;
          if (b2 <= 0) {
            continue;
          }
          for (int d2 = 1; d2 < 180 - a1 - a2; d2++) {
            int d1 = 180 - d2 - a1 - a2;
            if (d1 <= 0) {
              continue;
            }
            int c3 = 180 - a1 - a2;
            double k = 1.0;
            double l = sines[b2] * k / sines[b1];
            double m = sines[a2] * l / sines[a1];
            double n = sines[d2] * m / sines[d1];
            double x = Math.sqrt(k * k + n * n - 2 * k * n * cosines[c3]);
            double inner = clamp(n * sines[c3] / x, -1, 1);
            double c1 = Math.toDegrees(Math.asin(inner));
            inner = clamp(k * sines[c3] / x, -1, 1);
            double c2 = Math.toDegrees(Math.asin(inner));
            if (isInteger(c1) && isInteger(c2)) {
              int intC1 = (int)(c1 + 0.5);
              int intC2 = (int)(c2 + 0.5);
              if ((b2 + intC1 >= 180) | (intC2 + d1 >= 180)) {
                continue;
              }
              if (a1 + a2 + b1 + b2 + intC1 + intC2 + d1 + d2 != 360) {
                continue;
              }
              Quadrilateral q = new Quadrilateral(a1, a2, b1, b2, intC1, intC2, d1, d2);
              solutions.add(q);
            }
          }
        }
      }
    }
    System.out.println(solutions.size());
  }

}
