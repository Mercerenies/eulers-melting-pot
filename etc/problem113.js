
// Several simplifications later, and the problem boils down to a
// polynomial calculation.

function evalAt(n, xs) {
  let result = 0;
  for (let i = xs.length - 1; i >= 0; i--) {
    result = result * n + xs[i];
  }
  return result;
}

let n = 100;

let coeffs = [1, -71 / 90, 4913 / 8400, 117697 / 181440, 34913 / 90720, 4703 / 34560, 5173 / 172800, 71 / 17280, 41 / 120960, 11 / 725760, 1 / 3628800];

console.log((evalAt(n + 1, coeffs) - 9 * n - 2));
