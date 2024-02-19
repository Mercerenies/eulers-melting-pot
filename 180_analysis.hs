
f1, f2, f3, f :: Int -> Rational -> Rational -> Rational -> Rational

f1 n x y z = x ^^ (n + 1) + y ^^ (n + 1) - z ^^ (n + 1)
f2 n x y z = (x * y + y * z + z * x) * (x ^^ (n - 1) + y ^^ (n - 1) - z ^^ (n - 1))
f3 n x y z = x * y * z * (x ^^ (n - 2) + y ^^ (n - 2) - z ^^ (n - 2))
f n x y z = f1 n x y z + f2 n x y z - f3 n x y z
