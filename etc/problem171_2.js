
// Same technique as problem171_1.js but we pre-compute the
// countRemainingDigitSums cache rather than doing it on-demand.

const LIMIT = BigInt(1000000000)

// The largest 20-digit number consists of twenty 9's in a row. The
// sum of squares of digits of that number is 81 * 20 = 1620.
// Therefore, the only perfect squares we can ever encounter are those
// less than 1620. The final such perfect square is 1600 (= 40 * 40).
// Enumerate these squares and put them in a dictionary.
const isPerfectSquare = {}
for (let i = 0; i <= 40; i++) {
  isPerfectSquare[i * i] = true;
}

function squareDigitSum(n) {
  let total = 0;
  while (n != 0) {
    total += (n % 10) * (n % 10);
    n = Math.floor(n / 10);
  }
  return total;
}

// Given a current sum, how many ways can we add 11 digits to it and
// get something whose sum-of-squares-of-digits is a perfect square?
const DIGIT_SUMS_ARRAY_LENGTH = 1621
remainingDigitSumsCount = Array(DIGIT_SUMS_ARRAY_LENGTH).fill(0);

// To start with, we could always just add a bunch of zeroes, so
// perfect squares are already valid solutions.
for (let i = 0; i <= 40; i++) {
  remainingDigitSumsCount[i * i] = 1;
}

// Now, we're going to iteratively add 11 digits to the array's
// information.
for (let i = 0; i < 11; i++) {
  // For each position of our array, consider how we could get to that
  // position by adding precisely one digit to an existing sum.
  for (let j = 0; j < DIGIT_SUMS_ARRAY_LENGTH; j++) {
    for (let digit = 1; digit < 10; digit++) {
      remainingDigitSumsCount[j] += (remainingDigitSumsCount[j + digit * digit] ?? 0);
    }
  }
}

let finalTotal = BigInt(0);
for (let i = 0; i < 1000000000; i++) {
  if (i % 10000000 == 0) console.log(i); // Need to see 100 of these
  const sumSoFar = squareDigitSum(i);
  const waysToAddUp = remainingDigitSumsCount[sumSoFar];
  finalTotal = (finalTotal + BigInt(waysToAddUp) * BigInt(i)) % LIMIT;
}
console.log(finalTotal);

// Runs in 4m40s, a slight improvement over problem171_1.js
