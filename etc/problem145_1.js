
// When considering the sum of a pair of digits, we only care about
// two things: whether the sum is an even digit, and whether the sum
// produces a "carry bit" or not. As such, there are four states.
//
// * OWC - Odd with carry
// * ONC - Odd with no carry
// * EWC - Even with carry
// * ENC - Even with no carry
//
// If we consider our resulting sum (n+reverse(n)) as a sequence whose
// digits are drawn from the above enumeration of states, then the
// requirement that the sum be written as n+reverse(n) is tantamount
// to requiring that the sum (written in this enumeration form) be a
// palindrome.
//
// The right-most digit must be an odd state, either OWC or ONC. When
// considering a digit other than the right-most digit, if the digit
// to its right did not have a carry bit (ENC or ONC), then the
// current digit must be odd (OWC or ONC). If the previous digit did
// have a carry bit (EWC or OWC) then the current digit must be even
// (EWC or ENC) to compensate for the carry bit. If the right-most
// digit is OWC or ONC and the other digits satisfy the property
// above, we say the number satisfies the chain condition.
//
// Enumerate all palindromes consisting of at most nine digits (4^9).
// We need not consider values with ten digits, as the only possible
// value for the tenth digit is 1, and that is captured by the carry
// states on the leftmost digit (EWC and OWC). Check that palindromes
// satisfy the above chain condition. If a palindrome does, then
// identify how many numbers fit that palindrome, and add it to our
// total.
//
// -------------------------
//
// Further analysis after noticing a pattern here:
//
// Since we're looking at palindromes, the compatibility condition
// (that OWC/EWC must be followed by ENC/EWC and ONC/ENC must be
// followed by ONC/OWC) must be symmetric. That is, an OWC/EWC must be
// surrounded on *both* sides by ENC/EWC, and an ONC/ENC must be
// surrounded on *both* sides by ONC/OWC. This narrows our search
// space considerably. In particular, after narrowing our
// compatibility relation to be symmetric, the *only* states that are
// compatible are ONC ~ ONC and OWC ~ ENC. I claim that there are only
// two possible types of numbers to consider.
//
// (1) Numbers consisting only of ONC, and
//
// (2) Numbers (of odd length) consisting of an alternating sequence
// of OWC and ENC, starting and ending with OWC.
//
// In particular, EWC is never a legal state.
//
// We can narrow this down further. A number of odd length can only
// ever have a middle digit in state ENC or EWC, since we had to have
// added the same number to itself twice to get that digit. We've
// already ruled out EWC as a legal state, so the only numbers of odd
// length are those of the form 1414...1 where `length % 4 == 3`. If
// `length % 4 == 1` then the number cannot satisfy our condition.
//
// So every number is either (1) a number of even length consisting of
// only ONC, or (2) a number of length 3 (mod 4) consisting of an
// alternating sequence of OWC/ENC.
//
// Now we can easily enumerate all palindromes of these forms. The
// only minor stumbling block is that we have to explicitly exclude
// any values ending in a zero, as the problem requires that n not
// have any leading or trailing zeroes.

const TOTAL_DIGITS = 9;

/*
const OWC = 1;
const ONC = 2;
const EWC = 3;
const ENC = 4;

function isCompatible(right, left) {
  if (right === undefined) {
    // We're looking at the rightmost digit, expect odd
    return (left == OWC) || (left == ONC);
  } else if ((right == ONC) || (right == ENC)) {
    // No carry, expect odd
    return (left == OWC) || (left == ONC);
  } else {
    // Carry, expect even
    return (left == EWC) || (left == ENC);
  }
}

// Generates the non-repeating part of a palindrome, given the desired
// total length (so for n = 5, this would produce 432 to represent
// 23432, for instance).
function allPalindromesHead(n) {
  if (n == 0) {
    return [[]];
  } else if (n == 1) {
    return [[OWC], [ONC], [EWC], [ENC]];
  } else {
    const rec = allPalindromesHead(n - 2);
    const all = [];
    for (let i = 1; i <= 4; i++) {
      for (const rest of rec) {
        all.push([...rest, i]);
      }
    }
    return all;
  }
}

function allPalindromes(n) {
  const palindromes = allPalindromesHead(n);
  // Now turn them into the actual palindromes.
  return palindromes.map(function(arr) {
    if (n % 2 == 1) {
      // Odd length palindrome
      return [...arr, ...arr.slice(0, -1).reverse()];
    } else {
      // Even length palindrome
      return [...arr, ...arr.slice(0).reverse()];
    }
  });
}

function isValid(states) {
  // Check each digit in turn, from right to left.
  let lastState = undefined;
  for (let i = states.length - 1; i >= 0; i--) {
    const state = states[i];
    if (!isCompatible(lastState, state)) {
      return false;
    }
    lastState = state;
  }
  return true;
}

for (let i = 0; i <= TOTAL_DIGITS; i++) {
  for (const candidate of allPalindromes(i)) {
    if (isValid(candidate)) {
      console.log(candidate);
    }
  }
}
*/

// Precompute the number of values of a given state.
evenNoCarry = 0;
oddWithCarry = 0;
oddNoCarry = 0;
evenNoCarryZero = 0;
oddWithCarryZero = 0;
oddNoCarryZero = 0;
for (let i = 0; i <= 9; i++) {
  for (let j = 0; j <= 9; j++) {
    const sum = i + j;
    if ((sum % 2 == 0) && (sum >= 10)) {
      // EWC, illegal state; don't save
    } else if ((sum % 2 == 1) && (sum >= 10)) {
      if (i == 0 || j == 0) {
        oddWithCarryZero += 1;
      } else {
        oddWithCarry += 1;
      }
    } else if ((sum % 2 == 0) && (sum <= 10)) {
      if (i == 0 || j == 0) {
        evenNoCarryZero += 1;
      } else {
        evenNoCarry += 1;
      }
    } else if ((sum % 2 == 1) && (sum <= 10)) {
      if (i == 0 || j == 0) {
        oddNoCarryZero += 1;
      } else {
        oddNoCarry += 1;
      }
    }
  }
}

// Middle digit (of an odd-length number) must always be ENC and must
// be the sum of the same number twice. We can simply write these out
// rather than computing them. Possibilities are [0, 1, 2, 3, 4]
//
// Note: Middle digit can never be the leading or trailing digit, as
// there are no solutions of length 1 (this is trivial to see by
// direct computation.
middleDigit = 5;

// Given the length of the input number, return the total count of
// possible values for it.
function countsForLength(n) {
  if (n % 2 == 0) {
    // Even length, must be all ONC and rightmost cannot be zero.
    return (oddNoCarry + oddNoCarryZero) ** (n / 2 - 1) * oddNoCarry;
  } else if (n % 4 == 3) {
    // Odd length equal to 3 (mod 4), must be alternating OWC / ENC,
    // rightmost still cannot be zero.
    return (oddWithCarry + oddWithCarryZero) ** ((n - 3) / 4) * oddWithCarry * (evenNoCarry + evenNoCarryZero) ** ((n - 3) / 4) * middleDigit
  } else {
    // Odd length equal to 1 (mod 4). No solutions.
    return 0;
  }
}

let total = 0;
for (let i = 1; i <= TOTAL_DIGITS; i++) {
  total += countsForLength(i);
}
console.log(total);
