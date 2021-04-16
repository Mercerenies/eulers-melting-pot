
function isPrime(n) { // 階乘(甲)
  var i = 2; // 丁
  while (i < n) {
    if (n % i == 0) {
      return false;
    }
    i += 1;
  }
  return true;
}

function nextPrime(n) { // 乘階(甲)
  while (!isPrime(n)) {
    n += 1;
  }
  return n;
}

function powerMod(a, b, n) { // 乘乘(甲,己,丁)
  var result = 1; // 戊
  while (b > 0) {
    result = (result * a) % n;
    b -= 1;
  }
  return result;
}

var index = 1; // 丁
var current = 2; // 己
// 1e10 stored in 甲
while ((powerMod(current - 1, index, current * current) + powerMod(current + 1, index, current * current)) % (current * current) < 1e10) {
  // current * current stored in 甲甲
  // current + 1 stored in 辛
  // current - 1 stored in 己己
  // first powermod stored in 辛辛
  // second powermod stored in 辛己
  // sum of powermods stored in 己辛
  // cmp value stored in 甲甲甲
  index += 1;
  current = nextPrime(current + 1);
}
console.log(index);

