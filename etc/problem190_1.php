<?php

// Use Lagrange Multipliers. For a fixed m, we want to maximize
// the function f(x) = x1 * x2^2 * ... * xm^m subject to the
// constraint g(x) = m - x1 - ... - xm = 0.
//
// The Lagrangian is: L(x, λ) = f(x) + λ * g(x)
//
// dL/dλ = g(x) = 0
//
// dL/d(xi) = i f(x) / xi - λ = 0
//
// Rearrange the bottom equation to get: xi = i f(x) / λ
//
// g(x) = 0 tells us m - sum(xi) = 0, or m - sum(i f(x) / λ) = 0. So
//
// m = sum(i f(x) / λ)
//   = (f(x) / λ) sum(i)
//   = (f(x) / λ) (m (m + 1)) / 2
//
// Rearrange: f(x) = 2 λ / (m + 1). Plug that back into the equation
// for xi to get
//
// xi = (i / λ) (2 λ / (m + 1)) = 2 i / (m + 1).
//
// These xi are the optimal values. Then
//
// P = f(x) = product(i = 1 to m, (2 i / (m + 1)) ** i).

function p($m) {
    $prod = 1;
    for ($i = 1; $i <= $m; $i++) {
        $prod *= (2 * $i / ($m + 1)) ** $i;
    }
    return $prod;
}

$sum = 0;
for ($i = 2; $i <= 15; $i++) {
    $sum += floor(p($i));
}
echo $sum;
?>
