
// Optimizations on problem154.js

// Remember: Pascal's pyramid is just trinomial coefficient, which can be
// represented as the multinomial coefficient using factorials (just like in the
// binomial case).

function padicValuation(n, p) {
  let result = 0;
  while (n % p == 0) {
    n /= p;
    result += 1;
  }
  return result;
}


// For our purposes, we care about 2's and 5's, since we eventually want to
// check whether 10^12 (i.e. 2^12 * 5^12) divides evenly into our number. We can
// ignore all other prime factors.
const ONE = {
  two: 0,
  five: 0,
};

function to2and5(n) {
  return {
    two: padicValuation(n, 2),
    five: padicValuation(n, 5),
  };
}

// Take two numbers represented as prime products and multiply them.
function multiply(n, m) {
  return {
    two: n.two + m.two,
    five: n.five + m.five,
  };
}

// Precondition: m is a product of twos and fives (there are no other prime
// factors in the number used to generate it)
function divides(m, n) {
  return (m.two <= n.two) && (m.five <= n.five);
}

// Take two numbers represented as prime products and divide them. Precondition:
// m divides evenly into n.
function divide(n, m) {
  return {
    two: n.two - m.two,
    five: n.five - m.five,
  };
}

// Precompute all factorials up to our target.
const FACTORIALS = {0: ONE};
let acc = ONE;
for (let i = 1; i <= 200000; i++) {
  acc = multiply(acc, to2and5(i));
  FACTORIALS[i] = acc;
}

const TARGET = { // 10^12
  two: 12,
  five: 12,
};

const NUMERATOR = FACTORIALS[200000];

// Assume wlog k1 <= k2 <= k3, and fix for orderings once we find a
// match.
let result_count = 0;
for (let k1 = 0; k1 <= 200000; k1++) {
  for (let k2 = k1; k2 <= 200000; k2++) {
    const k3 = 200000 - k1 - k2;
    if (k3 < k2) {
      break;
    }
    const value = divide(NUMERATOR, multiply(FACTORIALS[k1], multiply(FACTORIALS[k2], FACTORIALS[k3])));
    if (divides(TARGET, value)) {
      // 6 solutions if k1, k2, k3 are distinct, 3 if two are same, 1
      // if all same.
      if (k1 == k3) { // We know they're ordered, so check bounds.
        result_count += 1;
      } else if ((k1 == k2) || (k2 == k3)) {
        // Don't need to check k1 == k3 since we know at least one is distinct.
        result_count += 3;
      } else {
        // All distinct
        result_count += 6;
      }
    }
  }
}
console.log(result_count);
