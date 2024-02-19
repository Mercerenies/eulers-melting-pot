
f1, f2, f3, f, f' :: Int -> Rational -> Rational -> Rational -> Rational

f1 n x y z = x ^^ (n + 1) + y ^^ (n + 1) - z ^^ (n + 1)
f2 n x y z = (x * y + y * z + z * x) * (x ^^ (n - 1) + y ^^ (n - 1) - z ^^ (n - 1))
f3 n x y z = x * y * z * (x ^^ (n - 2) + y ^^ (n - 2) - z ^^ (n - 2))
f n x y z = f1 n x y z + f2 n x y z - f3 n x y z

f' n x y z = (x + y + z) * (x ^^ n + y ^^ n - z ^^ n)

-- There's a missing exponent. f1 is (n + 1) powers and the secondary
-- term involves choosing 0 terms. f2 is (n - 1) powers and the
-- secondary term involves choosing 2 terms. f3 is (n - 2) powers and
-- the secondary term involves choosing 3 terms. We don't have the
-- case where we have n powers and choose 1 term. Define f' to be the
-- missing function.
--
-- I hypothesize, though I don't know the name of this theorem, that, for all n,
--
-- f1 - f' + f2 - f3 = 0
--
-- So if we want f1 + f2 - f3 = 0, equivalently we can just check that
-- f' = 0.
--
-- And f' is really nice. It's a product, so we just need one of the
-- two factors to be zero. The first factor is (x + y + z), which is
-- never zero since x, y, and z are all positive.
--
-- The second factor is x^n + y^n - z^n = 0, or equivalently x^n + y^n
-- = z^n. By (a corollary to) Fermat's Last Theorem, there are no
-- solutions to this if n >= 3.
--
-- Split into the remaining cases for n.
--
-- If n = 0, this tells us that 2 = 1, a contradiction. No solutions.
--
-- If n = 1, we get x + y = z, which is a linear equation and easily
-- solvable.
--
-- If n = 2, we're just looking for Pythagorean triples, albeit with
-- rational coefficients.
--
-- If n = -1, we get (1/x) + (1/y) = (1/z), or yz + xz = xy. Solving
-- for z gives us z = (xy)/(x + y).
--
-- If n = -2, we get (1/x^2) + (1/y^2) = (1/z^2), or y^2 z^2 + x^2 z^2
-- = x^2 y^2. Solving for z gives us z = sqrt((xy)^2 / (x^2 + y^2)).
--
-- If n = -3, then by the same logic we get y^3 z^3 + x^3 z^3 = x^3
-- y^3. There are, again, no solutions by Fermat's Last Theorem, and
-- the same is true of n < -3.
