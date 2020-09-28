
const fs = require('fs');
const lines = fs.readFileSync('./files/p099_base_exp.txt', 'utf8').split("\n");

let biggest = 0;
let biggest_line = 0;
let current_line = 1;
for (let i = 0; i < 2000; i += 2) {
  let [a, b] = [lines[i], lines[i + 1]]
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

TOP
current_line
biggest_line
biggest
BOTTOM

At label skip-save
TOP
current_line
biggest_line
biggest
BOTTOM

At label next-iter
TOP
current_line
biggest_line
biggest
BOTTOM

At label loop-start
TOP
current_line
biggest_line
biggest
BOTTOM

*/
