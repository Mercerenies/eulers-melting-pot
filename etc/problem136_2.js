
// See problem136_1.js for details on the math here. Summary: Solve
// (3m - x) (x + m) = n.

const LIMIT = 50000000;

const solutions = Array(LIMIT).fill(0);

for (let x = 1; x < LIMIT; x++) {
  for (let m = Math.floor(x / 3 + 1); m <= LIMIT; m++) {
    const n = (3 * m - x) * (x + m);
    if (n >= LIMIT) {
      break;
    }
    if (n > 0) {
      solutions[n]++;
    }
  }
}

let singular_solutions = 0;
for (let i = 0; i < LIMIT; i++) {
  if (solutions[i] == 1) {
    singular_solutions++;
  }
}
console.log(singular_solutions);
