
const absentOrOnTimeCache = {};

// Ignores the possibility of a "late" student. This returns the
// number of strings of length exactly `length` which do NOT consist
// of three adjacent absences.
function absentOrOnTime(length) {
  if (length in absentOrOnTimeCache) {
    return absentOrOnTimeCache[length];
  }
  absentOrOnTimeCache[length] = absentOrOnTimeImpl(length);
  return absentOrOnTimeCache[length];
}

function absentOrOnTimeImpl(length) {
  if (length < 0) {
    return 0;
  } else if (length == 0) {
    return 1;
  } else if (length == 1) {
    return 2;
  } else if (length == 2) {
    return 4;
  } else {
    // Consider the last few characters in the string.
    let sum = 0;
    // Could be "O".
    sum += absentOrOnTime(length - 1);
    // Could be "OA".
    sum += absentOrOnTime(length - 2);
    // Could be "OAA".
    sum += absentOrOnTime(length - 3);
    // But can't be "AAA", so stop here.
    return sum;
  }
}

// This counts all of the ways we can place *exactly* one "late" among
// a sequence of "absent" and "on-time" values.
function countWithLate(length) {
  let sum = 0;
  for (let latePos = 0; latePos < length; latePos++) {
    sum += absentOrOnTime(latePos) * absentOrOnTime(length - latePos - 1);
  }
  return sum;
}

// We add together countWithLate (the number of possible ways to be
// late once) plus the number of ways to never be late. This is our
// final answer.
function countAll(length) {
  return countWithLate(length) + absentOrOnTime(length);
}

for (let i = 1; i <= 50; i++) {
  console.log(i + " " + countAll(i));
}
