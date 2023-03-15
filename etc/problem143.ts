
const LIMIT: i32 = 120000;

function gcd(a: i32, b: i32): i32 {
  while (b != 0) {
    const tmp = a;
    a = b;
    b = tmp % b;
  }
  return a;
}

function appendPair(map: Array<Set<i32>>, a: i32, b: i32): void {
  map[a].add(b);
}

export function main(): i32 {
  const assoc = new Array<Set<i32>>(LIMIT + 1);
  for (let i = 0; i < LIMIT + 1; i++) {
    assoc[i] = new Set<i32>();
  }

  for (let i = 1; i < LIMIT; i++) {
    const maxJ = Math.min(2*i, LIMIT);
    for (let j = i + 1; j < maxJ; j++) {
      let x = j ** 2 - i ** 2;
      let y = j * (2 * i - j);
      let divisor = gcd(x, y);
      x /= divisor;
      y /= divisor;
      if (x + y > LIMIT) {
        break;
      }
      const dx = x;
      const dy = y;
      while (x + y <= LIMIT) {
        appendPair(assoc, x, y);
        appendPair(assoc, y, x);
        x += dx;
        y += dy;
      }
    }
  }
  return 100;
}
