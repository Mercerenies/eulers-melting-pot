
// Same logic as problem168_1.raku but limiting myself to 32-bit
// integers. Anything bigger than that is stored as a string and done
// by hand.

const UPPER_LIMIT_DIGIT_COUNT = 100;
const MODULO_VALUE = 100000;

// Takes a string representing a base 10 number and shifts it.
function doShift(n) {
  return n.slice(-1) + n.slice(0, -1);
}

// Takes two strings representing base 10 numbers and adds them.
function sum(s, t) {
  let totalSum = "";
  let i = 1;
  let carry = 0;
  while (i <= Math.max(s.length, t.length)) {
    const sDigit = s[s.length-i] ?? 0;
    const tDigit = t[t.length-i] ?? 0;
    const smallSum = "" + (+sDigit + +tDigit + carry);
    totalSum = smallSum[smallSum.length - 1] + totalSum;
    if (smallSum.length > 1) {
      carry = +smallSum[0];
    } else {
      carry = 0;
    }
    i++;
  }
  if (carry > 0) {
    return "" + carry + totalSum;
  } else {
    return totalSum;
  }
}

// Add zeroes to the right side.
function padRight(s, len) {
  for (let i = 0; i < len; i++) {
    s += "0";
  }
  return s;
}

function repeatDigit(digit, len) {
  let s = "";
  for (let i = 0; i < len; i++) {
    s += digit;
  }
  return s;
}

// Takes s (a string representing a number) and n (an ordinary number)
function product(s, n) {
  let totalSum = "0";
  for (let digitIndex = 0; digitIndex < s.length; digitIndex++) {
    const digit = +s[s.length - digitIndex - 1];
    totalSum = sum(totalSum, padRight("" + (digit * n), digitIndex));
  }
  return totalSum;
}

function generateParasiticNumber(n, k) {
  let digits = 1;
  while (doShift(k) != product(k, n)) {
    const newProduct = product(k, n);
    const newDigit = newProduct[newProduct.length - digits] ?? '0';
    k = newDigit + k;
    digits++;
  }
  return k;
}

function countAllOneParasitic() {
  let totalSum = 0;
  for (let digit = 1; digit <= 9; digit++) {
    totalSum += +repeatDigit(digit, 2) + +repeatDigit(digit, 3) + +repeatDigit(digit, 4);
    totalSum += +repeatDigit(digit, 5) * 96;
    totalSum %= MODULO_VALUE;
  }
  return totalSum;
}

function countAll(n) {
  let totalSum = 0;
  for (let k = n; k <= 9; k++) {
    const parasiticNumber = generateParasiticNumber(n, ''+k);
    const repeatCount = Math.floor(UPPER_LIMIT_DIGIT_COUNT / parasiticNumber.length);
    totalSum += +product(parasiticNumber, repeatCount).slice(-5)
  }
  return totalSum;
}

function run() {
  let totalSum = 0;
  totalSum += countAllOneParasitic() % MODULO_VALUE;
  for (let n = 2; n <= 9; n++) {
    totalSum += countAll(n) % MODULO_VALUE;
  }
  return totalSum % MODULO_VALUE;
}

console.log(run());

