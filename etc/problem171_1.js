
// Okay, this one is for real.
//
// I iterate over all numbers up to nine-digits. For each number, I
// calculate the number of possible placements of the remaining 11
// digits that satisfy the problem condition (sum of squares of digits
// is a perfect square), and I multiply the number of placements by
// the original value itself. Sum all of those quantities to get the
// result. countRemainingDigitSums uses a caching dictionary to store
// old results.
//
// Takes five minutes to run and requires BigInt, but it technically
// runs. :D

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

// Caching dictionary for countRemainingDigitSums
const cache = {}

function countRemainingDigitSums(sumSoFar, remainingDigits) {
  const cacheKey = sumSoFar * 100 + remainingDigits; // remainingDigits < 100, so this is safe.
  if (cacheKey in cache) {
    return cache[cacheKey];
  }
  if (sumSoFar > 1600) {
    return 0;
  }
  if (remainingDigits <= 0) {
    return (isPerfectSquare[sumSoFar] ? 1 : 0);
  }
  let total = 0;
  for (let i = 0; i < 10; i++) {
    total += countRemainingDigitSums(sumSoFar + i * i, remainingDigits - 1);
  }
  cache[cacheKey] = total;
  return total;
}

let finalTotal = BigInt(0);
for (let i = 0; i < 1000000000; i++) {
  if (i % 10000000 == 0) console.log(i); // Need to see 100 of these
  const sumSoFar = squareDigitSum(i);
  const waysToAddUp = countRemainingDigitSums(sumSoFar, 11);
  finalTotal = (finalTotal + BigInt(waysToAddUp) * BigInt(i)) % LIMIT;
}
console.log(finalTotal);
