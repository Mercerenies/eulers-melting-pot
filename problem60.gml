
#define main

argmax = -1;
N = 5;

primality[0] = 0;
primes[0] = 0;
input[0] = 0;

cheap_primality = ds_map_create();

bound = 2000;
result = bound * N;

while (result >= bound * N) {
    bound *= 5;
    scr_sieve(max(bound * (N + 1), 99999999));
    result = scr_seek_seq(0);
}
show_message(result);

#define scr_is_prime_impl
var n = argument0;
for (var i = 2; i < n / 2; i++) {
    if (n mod i == 0) {
        return false;
    }
}
return true;

#define scr_sieve

var n = argument0;
var size = array_length_1d(primes);
primality[n - 1] = false;
for (var i = 2; i < n; i++) {
    if (!primality[i]) {
        primes[size++] = i;
        for (var j = 2 * i; j < n; j += i) {
            primality[j] = true;
        }
    }
}

#define scr_is_prime

var n = argument0;
if (n < array_length_1d(primality)) {
    return !primality[n];
} else {
    if (!ds_map_exists(cheap_primality, n)) {
        var result = scr_is_prime_impl(n);
        cheap_primality[? n] = result;
        return result;
    }
    return cheap_primality[? n];
}

#define scr_is_valid

var n = argument0;
var m = argument1;
if (!scr_is_prime(real(string(n) + string(m))))
   return false;
if (!scr_is_prime(real(string(m) + string(n))))
   return false;
return true;

#define scr_check

var upto = argument0;
for (var i = 0; i < upto - 1; i++) {
    for (var j = i + 1; j < upto; j++) {
        if (!scr_is_valid(primes[ input[i] ], primes[ input[j] ]))
            return false;
    }
}
return true;

#define scr_seek_seq

var start = argument0;
if (!scr_check(start))
    return bound * N;
if (start == N) { // ^.^
    var sum = 0;
    for (var i = 0; i < N; i++) {
        sum += primes[ input[i] ];
    }
    if (argmax == -1 || argmax > sum)
        argmax = sum;
    return sum;
}
var ind0 = (start == 0 ? 0 : input[start - 1] + 1);
var sum = bound * N;
for (var i = ind0; primes[i] < bound; i++) {
    if ((argmax >= 0) && (primes[i] > argmax))
        continue;
    input[start] = i;
    var curr = scr_seek_seq(start + 1);
    if (curr < sum)
        sum = curr;
}
return sum;
