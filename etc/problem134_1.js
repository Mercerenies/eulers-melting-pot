
function extendedGCD(a, b) {
  let old_r = a;
  let r = b;
  let old_s = 1;
  let s = 0;
  let old_t = 0;
  let t = 1;

  while (r != 0) {
    quotient = Math.floor(old_r / r);
    [old_r, r] = [r, old_r - quotient * r];
    [old_s, s] = [s, old_s - quotient * s];
    [old_t, t] = [t, old_t - quotient * t];
  }

  return [old_s, old_t];

}

function inverseOf(a, n) {
  const [x, _] = extendedGCD(a, n);
  return (x % n + n) % n;
}

function smallestMatch(p1, p2) {
  const m = 10 ** Math.ceil(Math.log10(p1));
  const inv = inverseOf(p2, m);
  return ((p1 * inv) % m) * p2;
}

sieve = [];
primes = [];
for (let i = 0; i < 1000004; i++) {
  sieve.push(true);
}
sieve[0] = false;
sieve[1] = false;
for (let i = 2; i < 1000004; i++) {
  if (sieve[i]) {
    primes.push(i);
    for (let j = i + i; j < 1000004; j += i) {
      sieve[j] = false;
    }
  }
}

let sum = BigInt(0);
for (let idx = 2; idx < primes.length - 1; idx++) {
  let p1 = primes[idx];
  let p2 = primes[idx + 1];
  sum += BigInt(smallestMatch(p1, p2));
}
console.log(sum);
