
// Same initial technique as problem171_2.js to generate the
// remainingDigitSumsCount array. But then we use combinatorics at the
// end to avoid enumerating all possible numbers.

const LIMIT = 1000000000
const DIGITS_IN_NUMBER = 9

function factorial(n) {
  let total = 1;
  for (let i = 1; i <= n; i++) {
    total *= i;
  }
  return total;
}

function multinomial(n, ks) {
  let remainder = n;
  let total = factorial(n);
  for (let i = 0; i < ks.length; i++) {
    total /= factorial(ks[i]);
    remainder -= ks[i];
  }
  total /= factorial(remainder);
  return total;
}

function digitCountsToSum(counts) {
  let total = 0;
  for (let i = 0; i < counts.length; i++) {
    const sqr = (i + 1) * (i + 1);
    total += counts[i] * sqr;
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

// We're going to recursively determine the number of ones, twos,
// threes, ..., nines in the number. By process of elimination, the
// remaining digits are zero. Once we know the quantity of each digit,
// we can use combinatorics to determine how many ways to arrange
// those digits into an (at most) nine-digit number.
function run(digitCounts, index, runningCount) {
  // Precondition: runningCount <= DIGITS_IN_NUMBER

  if (index >= digitCounts.length) {
    // We've figured out our digits. How many ways can we form a
    // number using these digits?
    const currentSum = digitCountsToSum(digitCounts);
    const arrangements = remainingDigitSumsCount[currentSum];
    return sumPossibilities(digitCounts, arrangements);
  }

  let total = 0;
  for (let i = 0; i <= DIGITS_IN_NUMBER - runningCount; i++) {
    digitCounts[index] = i;
    total = (total + run(digitCounts, index + 1, runningCount + i)) % LIMIT;
  }
  return total;
}

function sumPossibilities(digitCounts, higherArrangementsCount) {
  let total = 0;
  // Consider each digit in isolation and consider each possible value
  // for that digit.
  for (let positionInNumber = 0; positionInNumber < DIGITS_IN_NUMBER; positionInNumber++) {
    for (let i = 1; i < 10; i++) {
      if (digitCounts[i - 1] > 0) {
        const constant = i * 10 ** (positionInNumber);
        digitCounts[i - 1] -= 1;
        const lowerArrangementsCount = multinomial(DIGITS_IN_NUMBER - 1, digitCounts);
        digitCounts[i - 1] += 1;
        total = (total + ((((constant % LIMIT) * (lowerArrangementsCount % LIMIT)) % LIMIT) * (higherArrangementsCount % LIMIT)) % LIMIT) % LIMIT;
      }
    }
  }
  return total
}

function runAll() {
  return run(Array(9).fill(0), 0, 0);
}

console.log(runAll());
//console.log(sumPossibilities([2, 2, 0], 1));
//console.log(multinomial(6, [2, 2, 2]));

// 0.9s in Javascript. It's great!
