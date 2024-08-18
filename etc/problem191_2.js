
// Same approach as problem191_1.js, but we shift indices just a bit
// to avoid special cases on the second loop. Just simplifying the
// code for our target language as much as humanly possible.

// Same approach as problem191.js, but we pre-allocate the
// absentOrOnTime cache as an array.

const absentOrOnTime = Array(31).fill(0);
absentOrOnTime[0] = 1;
absentOrOnTime[1] = 2;
absentOrOnTime[2] = 4;
for (let i = 3; i < 31; i++) {
  absentOrOnTime[i] = absentOrOnTime[i - 1] + absentOrOnTime[i - 2] + absentOrOnTime[i - 3];
}
console.log(absentOrOnTime);

let sum = absentOrOnTime[30];
for (let latePos = 0; latePos < 15; latePos++) { // Problem is symmetrical: Go to 15 but double each value.
  sum += 2 * absentOrOnTime[latePos] * absentOrOnTime[29 - latePos];
}
console.log(sum);

// Here's my plan for Underload. I'm confident we can get the
// algorithm, but I'm NOT confident we can extract a numerical result.
//
// Write `a` as shorthand for `absentOrOnTime`. Then we want to
// calculate `a[30] + 2 * (a[0] * a[29] + a[1] * a[28] + a[2] * a[27]
// + ... + a[14] * a[15])`.
//
// So, proceed as follows.
//
// 1. Generate a[0], a[1], a[2] on the stack by hand. Easy.
//
// 2. Generate a[3] through a[14] on the stack by adding previous
// stack elements.
//
// 3. We've hit the halfway point. Now generate a[15], a[16], and
// a[17] on the stack.
//
// 4. For a[18], generate the value as usual. Then go back down to
// a[14] and a[15] and multiply them. Also multiply by 2 at this time.
// At this point, the stack, from the top, looks like a[18], a[17], a[16],
// 2*a[15]*a[14], a[13], ... a[0].
//
// 5. For a[19], generate the value, then go down on the stack,
// multiply two stack values, and add them to the accumulated sum.
// After this step, the stack looks like a[19], a[18], a[17],
// 2*a[15]*a[14] + 2*a[16]*a[13], ... a[0].
//
// 6. Do the same thing we did in Step 5 for a[20] through a[29]. The
// stack now shows: a[29], a[28], a[27]. accum_sum, a[2], a[1], a[0].
//
// 7. Generate a[30]. Now multiply the remaining stack elements (aside
// from a[30]) together. We get a stack of just two values: a[30],
// accum_sum.
//
// 8. Add the two values, and pray that we can find a way to output
// the number.

// Underload didn't pan out: too slow. Trying Emoticon now, and we'll
// write the code basically like the above code.
