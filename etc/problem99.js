
const fs = require('fs');
const lines = fs.readFileSync('./files/p099_base_exp.txt', 'utf8').split("\n");

let biggest = 0;
let biggest_line = 0;
let current_line = 1;
for (const line of lines) {
  let [a, b] = line.split(",");
  a = parseInt(a);
  b = parseInt(b);
  const value = Math.log(a) * b;
  if (value > biggest) {
    biggest = value;
    biggest_line = current_line;
  }
  current_line++;
}
console.log(biggest_line);

/*
 * 0x1 Output buffer
 * 0x2 File buffer
 * 0x3 Variable a
 * 0x4 Variable b
 * 0x5 Temporary variable i
 * 0x6 Temporary variable j
 * 0x7 Temporary variable k
 */
