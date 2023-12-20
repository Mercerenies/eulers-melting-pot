
// I'm refining problem171_3.js to look more like the Fish shell
// solution, so we can debug why the latter is off by 1024.

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

function countFor(constant, modifiedDigitCounts, higherArrangementsCount) {
  const lowerArrangementsCount = multinomial(DIGITS_IN_NUMBER - 1, modifiedDigitCounts);
  return ((((constant % LIMIT) * (lowerArrangementsCount % LIMIT)) % LIMIT) * (higherArrangementsCount % LIMIT)) % LIMIT;
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
        const amount = countFor(constant, digitCounts, higherArrangementsCount);
        digitCounts[i - 1] += 1;
        total = (total + amount) % LIMIT;
      }
    }
  }
  return total
}

let total = 0;

for (let ones = 0; ones <= 9; ones++) {
  for (let twos = 0; twos <= 9 - ones; twos++) {
    for (let threes = 0; threes <= 9 - ones - twos; threes++) {
      for (let fours = 0; fours <= 9 - ones - twos - threes; fours++) {
        for (let fives = 0; fives <= 9 - ones - twos - threes - fours; fives++) {
          for (let sixes = 0; sixes <= 9 - ones - twos - threes - fours - fives; sixes++) {
            for (let sevens = 0; sevens <= 9 - ones - twos - threes - fours - fives - sixes; sevens++) {
              for (let eights = 0; eights <= 9 - ones - twos - threes - fours - fives - sixes - sevens; eights++) {
                for (let nines = 0; nines <= 9 - ones - twos - threes - fours - fives - sixes - sevens - eights; nines++) {
                  const arrangements = remainingDigitSumsCount[1 * ones + 4 * twos + 9 * threes + 16 * fours + 25 * fives + 36 * sixes + 49 * sevens + 64 * eights + 81 * nines];
                  for (let positionInNumber = 0; positionInNumber < DIGITS_IN_NUMBER; positionInNumber++) {
                    let amount = 0;
                    // Ones
                    if (ones > 0) {
                      total = (total + ((((((40320 / ( factorial(ones - 1) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (1 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Twos
                    if (twos > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos - 1) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (2 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Threes
                    if (threes > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes - 1) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (3 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Fours
                    if (fours > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours - 1) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (4 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Fives
                    if (fives > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives - 1) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (5 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Sixes
                    if (sixes > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes - 1) * factorial(sevens) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (6 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Sevens
                    if (sevens > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens - 1) * factorial(eights) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (7 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Eights
                    if (eights > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights - 1) * factorial(nines) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (8 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                    // Nines
                    if (nines > 0) {
                      total = (total + ((((((40320 / ( factorial(ones) * factorial(twos) * factorial(threes) * factorial(fours) * factorial(fives) * factorial(sixes) * factorial(sevens) * factorial(eights) * factorial(nines - 1) * factorial(9 - ones - twos - threes - fours - fives - sixes - sevens - eights - nines))) % LIMIT) * (9 * 10 ** positionInNumber)) % LIMIT) * (arrangements % LIMIT)) % LIMIT)) % LIMIT
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
console.log(total);

// I didn't do (arrangements % LIMIT) so we were overflowing what we
// could precisely represent in our numerical representation. I guess
// Fish uses double-precision floats too. I'm sure glad I used JS to
// do this so the problem was reproducible.
