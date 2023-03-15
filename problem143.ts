
function gcd(a: number, b: number): number {
  while (b != 0) {
    [a, b] = [b, a % b];
  }
  return a;
}

function main(): number {
  const LIMIT = 120000;
  const pairs = {};

  for (let i = 1; i < LIMIT; i++) {
    const maxJ = Math.min(2 * i, LIMIT);
    for (let j = i + 1; j < maxJ; j++) {
      let x = j ** 2 - i ** 2;
      let y = j * (2 * i - j);
      const divisor = gcd(x, y);
      x /= divisor;
      y /= divisor;
      const dx = x;
      const dy = y;
      if (x + y > LIMIT) {
        break;
      }
      while (x + y <= LIMIT) {
        (pairs[x] ||= new Set<number>()).add(y);
        (pairs[y] ||= new Set<number>()).add(x);
        x += dx;
        y += dy;
      }
    }
  }

  const matches = new Set<number>();

  for (const pStr in pairs) {
    const p = +pStr;
    for (const r of pairs[p]) {
      for (const q of pairs[r]) {
        if ((pairs[p].has(q)) && (p + q + r <= LIMIT)) {
          matches.add(p + q + r);
        }
      }
    }
  }

  let total = 0;
  matches.forEach(function(x) {
    total += x;
 });
  return total;
}

console.log(main());
