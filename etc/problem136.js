
// We want to find x^2 - y^2 - z^2 = n
//
// By substituting x = y + m and z = y - m, we find
//
// y (y - 4 m) = - n
//
// With the constraints that 3 m < y < 6 m
//
// So let's start with that.

// Terminates in about 4 mins but isn't correct? :(
//
// Gets 826152

const UPPER_LIMIT = 15000000;

function* factors(n) {
  // We know n < 3 y^2 and since y is a decreasing quantity in this
  // loop, we can stop when we get below this quantity.
  for (let i = 1; i * i < 3 * n; i++) {
    if (n % i == 0) {
      yield i;
    }
  }
}

function solutions(n) {
  let solutionCount = 0;
  for (const coY of factors(n)) {
    const y = n / coY;
    // We want y - 4 m = - n / y
    if ((y * y + n) % (4 * y) == 0) {
      const m = y / 4 + n / (4 * y)
      if ((m < y) && (y < 4 * m)) {
        solutionCount++;
        if (solutionCount > 1) {
          // We only care if there's a unique solution, so if we've
          // found two, then short-circuit.
          break;
        }
      }
    }
  }
  return solutionCount;
}

let totalValues = 0;
for (let n = 1; n < UPPER_LIMIT; n++) {
  if (n % 10000 == 0) {
    console.log(n);
  }
  const solutionCount = solutions(n);
  if (solutionCount == 1) {
    totalValues++;
  }
}
console.log(totalValues);
