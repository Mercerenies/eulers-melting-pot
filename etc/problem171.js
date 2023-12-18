
// Just quick testing to get a feel for the values.

function f(n) {
  let total = 0;
  while (n != 0) {
    total += (n % 10) * (n % 10);
    n = Math.floor(n / 10);
  }
  return total;
}

function isPerfectSquare(x) {
  // Good enough for now
  return (x == 1) || (x == 4) || (x == 9) || (x == 16) || (x == 25) || (x == 36) || (x == 49) || (x == 64) || (x == 81) || (x == 100) || (x == 121) || (x == 144) || (x == 169);
}

function isFPerfectSquare(n) {
  return isPerfectSquare(f(n));
}

for (let i = 1; i < 1000; i++) {
  if (isFPerfectSquare(i)) {
    console.log(i);
  }
}
