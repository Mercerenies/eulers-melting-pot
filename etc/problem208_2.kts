
// Bottom-up but otherwise equivalent to problem208_1

class Problem208_2 {
  val memo: Array<Long> = Array(15 * 15 * 15 * 15 * 15 * 5) { -1L }

  fun run() {
    // Base cases
    for (dir in 0..4) {
      memo[dir] = 1
    }

    for (a in 0..14) {
      for (b in 0..14) {
        for (c in 0..14) {
          for (d in 0..14) {
            for (e in 0..14) {
              for (dir in 0..4) {
                if ((a == 0) && (b == 0) && (c == 0) && (d == 0) && (e == 0)) {
                  // This is a base case; we already set it
                  continue
                }
                val memoKey = dir + e * 5 + d * 75 + c * 1_125 + b * 16_875 + a * 253_125
                var sum = 0L
                when (dir) {
                  0 -> {
                    if (a > 0) {
                      sum += memo[memoKey - 253_125 + 1]
                    }
                    if (e > 0) {
                      sum += memo[memoKey - 5 + 4]
                    }
                  }
                  1 -> {
                    if (b > 0) {
                      sum += memo[memoKey - 16_875 + 1]
                    }
                    if (a > 0) {
                      sum += memo[memoKey - 253_125 - 1]
                    }
                  }
                  2 -> {
                    if (c > 0) {
                      sum += memo[memoKey - 1_125 + 1]
                    }
                    if (b > 0) {
                      sum += memo[memoKey - 16_875 - 1]
                    }
                  }
                  3 -> {
                    if (d > 0) {
                      sum += memo[memoKey - 75 + 1]
                    }
                    if (c > 0) {
                      sum += memo[memoKey - 1_125 - 1]
                    }
                  }
                  4 -> {
                    if (e > 0) {
                      sum += memo[memoKey - 5 - 4]
                    }
                    if (d > 0) {
                      sum += memo[memoKey - 75 - 1]
                    }
                  }
                }
                memo[memoKey] = sum
              }
            }
          }
        }
      }
    }

    val memoKey = 3_796_870 // = 14 * 5 + 14 * 75 + 14 * 1_125 + 14 * 16_875 + 14 * 253_125
    println(memo[memoKey])
  }

  // For reference:
  // val memoKey = dir + e * 5 + d * 75 + c * 1_125 + b * 16_875 + a * 253_125
  //
  // private fun computeImpl(a: Int, b: Int, c: Int, d: Int, e: Int, dir: Int): Long {
  //   when (dir) {
  //     0 -> return computeMemoized(a - 1, b, c, d, e, 1) + computeMemoized(a, b, c, d, e - 1, 4)
  //     1 -> return computeMemoized(a, b - 1, c, d, e, 2) + computeMemoized(a - 1, b, c, d, e, 0)
  //     2 -> return computeMemoized(a, b, c - 1, d, e, 3) + computeMemoized(a, b - 1, c, d, e, 1)
  //     3 -> return computeMemoized(a, b, c, d - 1, e, 4) + computeMemoized(a, b, c - 1, d, e, 2)
  //     4 -> return computeMemoized(a, b, c, d, e - 1, 0) + computeMemoized(a, b, c, d - 1, e, 3)
  //     else -> throw RuntimeException("invalid dir: $dir")
  //   }
  // }
}

Problem208_2().run()
