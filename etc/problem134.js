
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

function findMatchingNumber(p1, p2) {
  const p = BigInt(p2);
  let n = BigInt(p1);
  const incr = BigInt(10) ** BigInt(("" + p1).length)
  while (true) {
    n += incr;
    if (n % p == 0) {
      return n;
    }
  }
}

let sum = BigInt(0);
for (let idx = 2; idx < primes.length - 1; idx++) {
  let p1 = primes[idx];
  let p2 = primes[idx + 1];
  sum += findMatchingNumber(p1, p2);
}
console.log(sum);
