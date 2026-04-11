
// Observation 1: Every solution to Problem 208 must go in each of the
// five directions an equal number of times. The directions are 36
// degrees times N where N is 1, 3, 5, 7, or 9. Let a equal the number
// of times we went in dir 1, b for dir 2, c for dir 3, d for dir 4, e
// for dir 5.
//
// Looking at the Y axis first, we observe that in the basis {s, sc},
// those vectors are <1, 0>, <0, 2>, <0, 0>, <0, -2>, and <-1, 0>. It
// is straightforward to see that a = e and b = d from this.
//
// Now in the X direction. The vectors in the basis {c, c2} are <1,
// 0>, <-2, 2>, <2, -4>, <-2, 2>, <1, 0>. A "total" X position in this
// basis is
//
// 0 = a <1, 0> + b <-2, 2> + c <2, -4> + d <-2, 2> + e <1, 0>
//
// We already established a = e, b = d. Hence,
//
// 0 = a <2, 0> + b <-4, 4> + c <2, -4>
//
// Splitting that out,
//
// 0 = 2 a - 4 b + 2 c
// 0 = 4 b - 4 c
//
// Second formula readily gives us b = c. So
//
// 0 = 2 a - 2 b
//
// Hence, a = b, and all variables are equal.
//
// Observation 2: The converse is true. Let P be an arbitrary path of
// length 5k (where k is an integer of our choosing; in the problem, k
// = 14 so 5k = 70). Assume that P goes in each direction the same
// number of times (in particular, P goes in each direction k times).
// Then, in terms of final position, P is equivalent to a path that
// goes in k pentagons (simply by reordering the path elements). And a
// path that goes in k pentagons clearly ends up back at the starting
// position.
//
// Conclusion: A path is valid per this problem's description if and
// only if it travels in all of the directions exactly 14 times each.
// That's doable with array-based DP.
//
// 1.6 secs in Kotlin

class Problem208_1 {
  val memo: Array<Long> = Array(15 * 15 * 15 * 15 * 15 * 5) { -1L }

  fun run() {
    println(computeMemoized(14, 14, 14, 14, 14, 0))
  }

  private fun computeMemoized(a: Int, b: Int, c: Int, d: Int, e: Int, dir: Int): Long {
    if ((a < 0) || (b < 0) || (c < 0) || (d < 0) || (e < 0)) {
      return 0
    }
    if ((a == 0) && (b == 0) && (c == 0) && (d == 0) && (e == 0)) {
      return 1
    }
    val memoKey = dir + e * 5 + d * 75 + c * 1_125 + b * 16_875 + a * 253_125
    if (memo[memoKey] != -1L) {
      return memo[memoKey]
    }
    memo[memoKey] = computeImpl(a, b, c, d, e, dir)
    return memo[memoKey]
  }

  private fun computeImpl(a: Int, b: Int, c: Int, d: Int, e: Int, dir: Int): Long {
    when (dir) {
      0 -> return computeMemoized(a - 1, b, c, d, e, 1) + computeMemoized(a, b, c, d, e - 1, 4)
      1 -> return computeMemoized(a, b - 1, c, d, e, 2) + computeMemoized(a - 1, b, c, d, e, 0)
      2 -> return computeMemoized(a, b, c - 1, d, e, 3) + computeMemoized(a, b - 1, c, d, e, 1)
      3 -> return computeMemoized(a, b, c, d - 1, e, 4) + computeMemoized(a, b, c - 1, d, e, 2)
      4 -> return computeMemoized(a, b, c, d, e - 1, 0) + computeMemoized(a, b, c, d - 1, e, 3)
      else -> throw RuntimeException("invalid dir: $dir")
    }
  }
}

Problem208_1().run()
