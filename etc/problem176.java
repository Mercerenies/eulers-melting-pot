
// We know
// (https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple)
// that we can uniquely generate all Pythagorean triples as
//
// a = k (m^2 - n^2)
// b = k (2 m n)
// c = k (m^2 + n^2)
//
// Where k, m, n > 0; m > n; m and n coprime; and m and n not both
// odd.

import java.util.stream.LongStream;

public class problem176 {

  public static void main(String[] args) {
    long test = 1;
    while (true) {
      System.out.println(test + " " + countCatheti(test));
      if (countCatheti(test) == 47547L) {
        break;
      }
      test += 1;
    }
    System.out.println(test);
  }

  private static long countCatheti(long r) {
    return countFirstSide(r) + countSecondSide(r);
  }

  private static long countFirstSide(long r) {
    // r = k (m^2 - n^2)
    return factorsOf(r).map((k) -> {
      long count = 0;
      // m^2 - n^2 is at least m^2 - (m-1)^2, or 2m - 1. So once 2m -
      // 1 exceeds r / k, we're done. Add 1 (= 2/2) to the bound to
      // deal with truncation on longs.
      for (long m = lsqrt(r / k); m < ((r / k) + 3) / 2; m++) {
        // Solve for n^2 and see if it's a perfect square.
        long n2 = m * m - r / k;
        long n = lsqrt(n2);
        if ((n * n == n2) && (isValidConstantTriple(k, m, n))) {
          debugPrint(k, m, n);
          count += 1;
        }
      }
      return count;
    }).sum();
  }

  private static long countSecondSide(long r) {
    // r = k (2 m n)
    if (r % 2 == 1) {
      // The "b" side of a Pythagorean triple is necessarily even.
      return 0;
    }
    return factorsOf(r / 2).map((k) ->
      factorsOf(r / (2 * k)).filter((m) -> {
        long n = r / (2 * k * m);
        if (isValidConstantTriple(k, m, n)) {
          debugPrint(k, m, n);
        }
        return isValidConstantTriple(k, m, n);
      }).count()
    ).sum();
  }

  private static boolean isValidConstantTriple(long k, long m, long n) {
    return (n > 0) && (m > n) && ((m * n) % 2 == 0) && (gcd(m, n) == 1);
  }

  private static LongStream factorsOf(long n) {
    return LongStream
      .iterate(1, (d) -> d + 1)
      .takeWhile((d) -> d * d <= n)
      .flatMap((d) -> {
          if (n % d == 0) {
            if (d * d == n) {
              return LongStream.of(d);
            } else {
              return LongStream.of(d, n / d);
            }
          } else {
            return LongStream.empty();
          }
        });
  }

  private static long gcd(long a, long b) {
    while (b != 0) {
      long t = b;
      b = a % b;
      a = t;
    }
    return a;
  }

  private static long lsqrt(long x) {
    // This is not great but it'll do for a prototype. If we need to
    // write an actual integer square root algorithm, we will.
    return (long)Math.sqrt((double)x);
  }

  private static void debugPrint(long k, long m, long n) {
    long a = k * (m * m - n * n);
    long b = 2 * k * m * n;
    long c = k * (m * m + n * n);
    //System.out.printf("%d^2 + %d^2 = %d^2\n", a, b, c);
  }

}
