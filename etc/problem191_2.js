
// Same approach as problem191_1.js, but we shift indices just a bit
// to avoid special cases on the second loop. Just simplifying the
// code for our target language as much as humanly possible.

const absentOrOnTime = Array(32).fill(0);
absentOrOnTime[0] = 1; // ???
absentOrOnTime[1] = 1;
absentOrOnTime[2] = 2;
absentOrOnTime[3] = 4;
for (let i = 4; i < 32; i++) {
  absentOrOnTime[i] = absentOrOnTime[i - 1] + absentOrOnTime[i - 2] + absentOrOnTime[i - 3];
}
console.log(absentOrOnTime);

let sum = 0;
for (let latePos = 0; latePos < 31; latePos++) {
  sum += absentOrOnTime[latePos] * absentOrOnTime[31 - latePos];
}
console.log(sum);
