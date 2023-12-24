
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
import java.util.ArrayList;

public class problem176 {

  public static void main(String[] args) {
    long test = 3;
    while (true) {
      //System.out.println(test + " " + countCatheti(test) + " " + countCathetiFromPrimes(test));
      if (test % 1000 == 0) {
        System.out.println(test);
      }
      if (countCathetiFromPrimes(test) == 47547L) {
        break;
      }
      test += 1;
    }
    System.out.println(test);
  }

  private static long countCathetiFromPrimes(long r) {
    var valuations = primeValuations(r);
    long product = (valuations.get(0) > 0) ? (2 * valuations.get(0) - 1) : 1;
    for (int i = 1; i < valuations.size(); i++) {
      product *= (2 * valuations.get(i) + 1);
    }
    return (product - 1) / 2;
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

  private static ArrayList<Long> primeValuations(long n) {
    var result = new ArrayList<Long>();
    for (long i = 2; i <= n; i++) {
      long currentCount = 0;
      while (n % i == 0) {
        currentCount++;
        n /= i;
      }
      // Only include nonzero terms, but always include the i=2 term
      // since we treat that one differently in the product.
      if ((currentCount > 0) || (i == 2)) {
        result.add(currentCount);
      }
    }
    return result;
  }

}

// By the way, https://oeis.org/A046079. That's probably not actually
// going to help too much, but documenting it here just in case.
//
// Actually that works rather well. The OEIS page gives us this
// formula. Write n as a product of prime powers
//
// n = 2^a0 * p1^a1 * ... * pk^ak
//
// Note that we distinguish the case of p=2 for reasons that will
// become clear in a moment. Then the number of ways n can be the leg
// of a (not necessarily primitive) Pythagorean triple is
//
// [(2 * a0 - 1) (2 * a1 + 1) ... (2 * a(k-1) + 1) (2 * ak + 1) - 1] / 2
//
// Note that in the a0 case we subtract one rather than adding. If the
// number is odd, omit this case entirely. Let's see why that is.
//
// Proof: For now, assume n is odd. We'll discuss the even case in a
// moment. Write n = p1^a1 * ... * pk^ak.
//
// We want to write n^2 = p1^(2 a1) * ... pk^(2 ak) as the leg of a
// Pythagorean triple. That is, we want some positive c and b such
// that c^2 - b^2 = n^2. Put another way, (c + b)(c - b) = n^2.
// Substitute s = c + b, t = c - b and get n^2 = s t (with s > t both
// positive). That is, we want to know the number of ways to write n^2
// as a product of two terms.
//
// This is now a combinatorics problem. For each prime-powered factor
// pr^(2 ar), we have to make a decision of how many factors of pr to
// put into s and how many to put into t. There are (2 ar + 1) such
// arrangements. Do this for each prime-powered factor. Then we get (2
// * a1 + 1) (2 * a2 + 1) ... (2 * ak + 1) possible arrangements.
// Denote this product as Q.
//
// n^2 is a perfect square, so this count Q includes the case where s
// = t, i.e. the case where we perfectly evenly split every number.
// The resulting Pythagorean triple would have b=0, so we exclude this
// case, i.e. our count is now Q-1. Additionally, for every solution
// we counted where s > t, there's also one we counted where s < t
// (obtained by swapping all of the factors in the arrangement). So we
// double-counted every solution. Hence, the true count is (Q-1)/2,
// our formula.
//
// Now consider the case where n is even. A new problem arises in this
// case. Write
//
// n = 2^a0 * p1^a1 * ... * pk^ak
//
// where a0 > 0 by assumption. Using the same technique above, write
//
// n^2 = 2^(2 a0) * p1^(2 a1) * ... * pk^(2 ak)
//
// We want to write n^2 = s t, so there should be 2 a0 + 1 possible
// ways to arrange the factors of 2 which divide into n^2. But two of
// these arrangements should not be counted. When n was odd, s and t
// were also always odd (since no even number can divide into an odd
// number). If all of the factors of 2 go into s or all of them go
// into t, then s and t have opposite parity. Then when we try to find
// c and b such that s = c + b and t = c - b, we'll get non-integer
// values for c and b, which renders this not a valid Pythagorean
// triple.
//
// Hence, when placing the factors of 2 in s and t, we need to omit
// the cases where either s or t would receive all factors of 2. So
// the true count for the prime value 2 is in fact 2 a0 - 1, not 2 a0
// + 1. Beyond this, the counting technique works the same, as we can
// arrive at the same conclusion.
//
// QED
