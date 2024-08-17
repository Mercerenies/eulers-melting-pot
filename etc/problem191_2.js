
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
