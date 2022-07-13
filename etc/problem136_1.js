
// If we start with x, y, z as the (ascending) terms of the arithmetic
// sequence and write y = x + m and z = x + 2m, we find that z^2 - y^2
// - x^2 = n implies
//
// (3m - x) (x + m) = n
//
// Constraints: x > 0, m > 0
//
// Therefore, since x + m > 0 and n > 0, we have 3m > x
//
// Once we factor n into c×d, we find that m = (c+d)/4 and x=(3d-c)/4.

// Takes 20 mins but it gets there in the end. :)

function* yield_factors(n) {
  for (let i = 1; i * i <= n; i++) {
    if (n % i == 0) {
      yield [i, n / i];
    }
  }
}

function solve_for(c, d) {
  // Note: We have to have x > 0, hence 3d - c > 0. That explains the
  // third constraint here.
  return ((c + d) % 4 == 0) && ((3 * d - c) % 4 == 0) && (3 * d > c);
}

function solution_count(n) {
  let count = 0;
  for (const [c, d] of yield_factors(n)) {
    if (solve_for(c, d)) {
      count++;
    }
    if ((c != d) && (solve_for(d, c))) {
      count++;
    }
  }
  return count;
}

let total_count = 0;
for (let i = 1; i < 50000000; i++) {
  if (solution_count(i) == 1) {
    total_count += 1;
  }
}
console.log(total_count);
