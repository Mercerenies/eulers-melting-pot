
const fs = require('fs');
const lines = fs.readFileSync('./files/p102_triangles.txt', 'utf8').split("\n");

let count = 0;
for (let i = 0; i < 6000; i+=6) {
  const line = lines.slice(i, i+6);
  const [x1, y1, x2, y2, x3, y3] = line.map((x) => parseInt(x));
  // Find the Barycentric coordinates of (0, 0) wrt this triangle (I
  // don't bother to normalize the result because I only care about
  // the sign of the answer)
  let tri = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3);
  let u = (y2 - y3) * (- x3) + (x3 - x2) * (- y3);
  let v = (y3 - y1) * (- x3) + (x1 - x3) * (- y3);
  let w = tri - u - v;
  if (tri < 0) {
    tri *= -1;
    u *= -1;
    v *= -1;
    w *= -1;
  }
  if (u >= 0 && u <= tri && v >= 0 && v <= tri && w >= 0 && w <= tri) {
    count++;
  }
}
console.log(count);
