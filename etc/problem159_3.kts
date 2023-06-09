
// Modification of problem159_2.kt that changes the loop to run better
// in Pikt

// digitalSum = FF8822
fun digitalSum(x: Int): Int {
  // x = FFEE00
  if (x % 9 == 0) {
    return 9
  } else {
    return x % 9
  }
}

// myMax = EEEE00
fun myMax(a: Int, b: Int): Int {
  // a = 1111FF, b = 11EEFF
  if (a > b) {
    return a
  } else {
    return b
  }
}

// myMin = EEEE99
fun myMin(a: Int, b: Int): Int {
  // a = 1111FF, b = 11EEFF
  if (a > b) {
    return b
  } else {
    return a
  }
}

// LIMIT = FFFF44
val LIMIT = 1000000

// allSums = 11FFEE
// Constant 1 = 9191FF
val allSums = mutableListOf<Int>()
for (i in 1..LIMIT) {
  // i = 81FF81
  allSums.add(0)
}

// UPPER = FF44FF
// Constant 2 = 9292FF
val UPPER = LIMIT-1
for (i in 2..UPPER) {
  // i = 81FF81
  allSums[i] = myMax(allSums[i], digitalSum(i))
  // j = 81AA81
  var j = i + i
  // intermediate div = FFAAFF
  // stop = EE11EE
  val stop = myMin(i, LIMIT / i)
  for (n in 2..stop) {
    // n = 813381
    if (j >= LIMIT) {
      break
    }
    // intermediate sum = 33FF21
    allSums[j] = myMax(allSums[j], allSums[i] + allSums[n])
    j += i
  }
}

// sum = 0505FF
var sum = 0
for (i in 2..UPPER) {
  // i = 81FF81
  sum += allSums[i]
}
println(sum)
