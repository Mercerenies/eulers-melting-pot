
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

let sum = 0;
sum += absentOrOnTime[30];
for (let latePos = 0; latePos < 30; latePos++) {
  sum += absentOrOnTime[latePos] * absentOrOnTime[29 - latePos];
}
console.log(sum);
