
// Grid:
//
// a b c d
// e f g h
// i j k l
// m n o p
//
// The grid in question has a total of 10 sums. Four vertical, four
// horizontal, and two diagonal. We want all ten to be the same. Treat
// the problem as a linear algebra problem. That's nine equations (set
// nine of the sums to all be equal to the tenth one).
//
// I used `a + f + k + p` as the "main" sum to which all others were
// forced to be equal to. Set up a massive 17x9 matrix consisting of
// all of the equations in our 16-dimensional vector space.
//
// [  0  1  1  1  0 -1  0  0  0  0 -1  0  0  0  0 -1 |  0 ]
// [ -1  0  0  0  1  0  1  1  0  0 -1  0  0  0  0 -1 |  0 ]
// [ -1  0  0  0  0 -1  0  0  1  1  0  1  0  0  0 -1 |  0 ]
// [ -1  0  0  0  0 -1  0  0  0  0 -1  0  1  1  1  0 |  0 ]
// [  0  0  0  0  1 -1  0  0  1  0 -1  0  1  0  0 -1 |  0 ]
// [ -1  1  0  0  0  0  0  0  0  1 -1  0  0  1  0 -1 |  0 ]
// [ -1  0  1  0  0 -1  1  0  0  0  0  0  0  0  1 -1 |  0 ]
// [ -1  0  0  1  0 -1  0  1  0  0 -1  1  0  0  0  0 |  0 ]
// [ -1  0  0  1  0 -1  1  0  0  1 -1  0  1  0  0 -1 |  0 ]
//
// This is nine equations over a 16-dimensional space. If all
// equations are independent, we get a seven-dimensional kernel.
//
// Row reduce this banana.
//
// We find that the span of these nine equations is eight-dimensional,
// not nine. So the kernel will have 16 - 8 = 8 dimensions. One
// possible solution is:
//
// [  1  0  0  0  0  0  0 -1  0  0  0 -1  1  0  0  0 |  0 ]
// [  0  1  0  0  0  0  0 -1  0  1 -1 -1  1  1  0 -1 |  0 ]
// [  0  0  1  0  0  0  0  1  0 -1  1  1 -2 -1  0  0 |  0 ]
// [  0  0  0  1  0  0  0  1  0  0  0  1 -1 -1 -1  0 |  0 ]
// [  0  0  0  0  1  0  0  1  0 -1 -1  0  0  0  0  0 |  0 ]
// [  0  0  0  0  0  1  0  1  0  0  1  1 -2 -1 -1  0 |  0 ]
// [  0  0  0  0  0  0  1 -1  0  1  0 -1  1  0  0 -1 |  0 ]
// [  0  0  0  0  0  0  0  0  1  1  1  1 -1 -1 -1 -1 |  0 ]
//
// Independent variables: h, j, k, l, m, n, o, p
//
// a = h + l - m
// b = h - j + k + l - m - n + p
// c = - h + j - k - l + 2 m + n
// d = - h - l + m + n + o
// e = - h + j + k
// f = - h - k - l + 2 m + n + o
// g = h - j + l - m + p
// i = - j - k - l + m + n + o + p
//
// Now we brute force the independent variables to figure out what
// keeps all the dependent variables in the range 0..9. Do so in an
// order that allows us to short-circuit early, so start with {h, j,
// k} so we can check e immediately, then add {l, m} and check a, and
// so on.
//
// Runs instantaneously and produces a correct answer.

object Problem166 {

  fun run(): Int {
    var solutions = 0
    for (h in 0..9) {
      for (j in 0..9) {
        for (k in 0..9) {
          val e = - h + j + k
          if (!inBounds(e)) {
            continue
          }
          for (l in 0..9) {
            for (m in 0..9) {
              val a = h + l - m
              if (!inBounds(a)) {
                continue
              }
              for (p in 0..9) {
                val g = h - j + l - m + p
                if (!inBounds(g)) {
                  continue
                }
                for (n in 0..9) {
                  val b = h - j + k + l - m - n + p
                  if (!inBounds(b)) {
                    continue
                  }
                  val c = - h + j - k - l + 2 * m + n
                  if (!inBounds(c)) {
                    continue
                  }
                  for (o in 0..9) {
                    val d = - h - l + m + n + o
                    if (!inBounds(d)) {
                      continue
                    }
                    val f = - h - k - l + 2 * m + n + o
                    if (!inBounds(f)) {
                      continue
                    }
                    val i = - j - k - l + m + n + o + p
                    if (!inBounds(i)) {
                      continue
                    }
                    solutions += 1
                  }
                }
              }
            }
          }
        }
      }
    }
    return solutions
  }

  fun inBounds(value: Int): Boolean =
    value >= 0 && value <= 9

}

println(Problem166.run())
