
{-

X*Y*Z

Layer 1: X*Y*Z
Layer 2: 2*X*Y + 2*Y*Z + 2*X*Z
Layer 3: 



    ccc
   cbbbc
  cbaaabc
  cbaaabc
   cbbbc
    ccc

2x3
6
10
14

First added layer is n=0
2x + 2y + 4n

Now starting with x,y,z

Define f(x, y, n) = 2x + 2y + 4n (2D solution)

Then define g(x, y, z, n) as follows (3D solution)
g(x, y, z, 0) = z f(x, y, 0) + 2 x y
g(x, y, z, 1) = z f(x, y, 1) + 2 f(x, y, 0) + 2 x y
g(x, y, z, 2) = z f(x, y, 2) + 2 f(x, y, 1) + 2 f(x, y, 0) + 2 x y
...
g(x, y, z, n) = z f(x, y, n) + 2 sum(i=0..n-1, f(x, y, i)) + 2 x y

sum(i=0..n-1, f(x, y, i)) = sum(i=0..n-1, 2x + 2y + 4i)
                          = 2xn + 2yn + 4 sum(i=1..n-1, i)
                          = 2xn + 2yn + 2 n (n-1)
                          = 2 n (x + y + n - 1)

g(x, y, z, n) = z (2 x + 2 y + 4 n) + 2 x y + 4 n (x + y + n - 1)
              = 2 x z + 2 y z + 4 n z + 2 x y + 4 n (x + y + n - 1)
              = 2 (x z + y z + 2 n z + x y + 2 n (x + y + n - 1))
-}

{-

g(x, y, z, k) = 2 (x z + y z + 2 k z + x y + 2 k (x + y + k - 1))

c(n) = # of sol'ns to n = 2 (x z + y z + 2 k z + x y + 2 k (x + y + k - 1))

n/2 = x z + y z + 2 k z + x y + 2 k (x + y + k - 1)
n/2 = x z + y z + x y + 2 k (x + y + z + k - 1)

(n/2 - [x y + 2 k (x + y + k - 1)]) / (x + y + 2 k) = z

-}

f :: Integral i => i -> i -> i -> i
f x y n = 2 * x + 2 * y + 4 * n

g :: Integral i => i -> i -> i -> i -> i
g x y z n = z * f x y n + 2 * sum [f x y i | i <- [0..n-1]] + 2 * x * y

g' :: Integral i => i -> i -> i -> i -> i
g' x y z n = 2 * (x * z + y * z + 2 * n * z + x * y + 2 * n * (x + y + n - 1))
