
// Look at problem176.java for the formula and proof of correctness of
// that formula.
//
// The formula is as follows. For n = 2^a0 * p1^a1 * ... & pk^ak, the
// number of ways to write n as a leg of a Pythagorean triple is
//
// j = [(2 * a0 - 1) (2 * a1 + 1) ... (2 * a(k-1) + 1) (2 * ak + 1) - 1] / 2
//
// We want the minimal n such that j = 47547. Rearranging, we want
//
// 2 j + 1 = (2 * a0 - 1) (2 * a1 + 1) ... (2 * a(k-1) + 1) (2 * ak + 1)
//
// Where 2 j + 1 = 95095 = 5 * 7 * 11 * 13 * 19.
//
// So we want a product which multiplies to 95095, and we want the
// resulting n to be minimal.
//
// As we did before (in problem176.java), let us omit the even case
// for now and assume n is odd. It always makes sense to write our
// product in descending order, since this will produce the smallest n
// (since the smaller prime numbers come first when multiplying to get
// n). Furthermore, it never makes sense to "combine" factors
// unnecessarily. For instance, in the above product, we have the
// choice of either writing 11 and 13 as *different* factors (11 * 13
// = (2 * 5 + 1) * (2 * 6 + 1)) or as the same (11 * 13 = 143 = (2 *
// 71 + 1)). In the first case, we'll end up contributing p1^5 * p2^6
// to the product. In the latter case, we contribute p1^71 to the
// product. I think it's fairly clear why it benefits us to keep them
// separate.
//
// Now throw the even case back in. In principle, we should write the
// product as 95095 = (2 * 10 - 1) (2 * 6 + 1) (2 * 5 + 1) (2 * 3 + 1)
// (2 * 2 + 1). This gives us a candidate solution of n = 2^10 3^6 5^5
// 7^3 11^2 = 96818198400000. But we have to consider the other things
// we could do to the 2^a0 term.
//
// If we consider a number which is only divisible by 2 and one other
// prime number p, i.e. n = 2^a0 * p^ak, we can write (2 a0 - 1) (2 ak
// + 1) = R. Then swapping the two terms of the product gives us R =
// (2 (ak + 1) - 1) (2 (a0 - 1) + 1) = R, so the new value n' is n' =
// 2^(ak+1) * p^(a0-1).
//
// We want the smallest n, so it only benefits us to "swap" 2 with
// another value if n' < n. That is, 2^a0 p^ak < 2^(ak+1) p^(a0-1). Or
// 2^(a0-ak-1) < p^(ak-a0-1). Write w = ak-a0, so we want 2^(-w-1) <
// p^(w-1). Clearly, this is only true when w > 0, so ak > a0. Hence,
// even in the case of 2, it only makes sense to swap factor order
// when we're doing so in order to maintain a decreasing sequence.
// Thankfully, our "special case" of 2 isn't a special case at all. So
// our candidate solution of 96818198400000 is in fact the correct
// solution.


public class problem176_1 {

  public static void main(String[] args) {
    long solution = lpow(2, 10) * lpow(3, 6) * lpow(5, 5) * lpow(7, 3) * lpow(11, 2);
    System.out.println(solution);
  }

  private static long lpow(long a, long b) {
    // Naive linear-time multiplication. No need for repeated squaring
    // for numbers this small.
    long product = 1L;
    for (long i = 0L; i < b; i++) {
      product *= a;
    }
    return product;
  }

}
