{-# LANGUAGE ParallelListComp #-}

-- If we weren't crossing off zeroes, then to get the last 5 digits we
-- would do everything modulo 10^5. If we were crossing off at most
-- one zero per multiplication, we would need to go modulo 10^6 so we
-- don't lose info. We need to know how many zeroes we can cross off
-- in one iteration. That is, we want the worst case. We cross off a
-- zero when we've introduced a 10, more specifically when we've
-- introduced a 2 and a 5 (in either order). So check the p-adic
-- valuation for 2 and 5 of each value.

import Data.List

pAdic :: Integral i => i -> i -> Int
pAdic x p
  | x `mod` p == 0 = 1 + pAdic (x `div` p) p
  | otherwise = 0

valuation :: Integral i => i -> (Int, Int)
valuation x = (x `pAdic` 2, x `pAdic` 5)

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) .+ (a', b') = (a + a', b + b')

-- Argument to mapAccumL
addAndSimplify :: (Int, Int) -> (Int, Int) -> ((Int, Int), Int)
addAndSimplify lhs rhs =
    let (a'', b'') = lhs .+ rhs
        m = min a'' b'' in
    ((a'' - m, b'' - m), m)

main :: IO ()
main = do
  let (_, cancels) = mapAccumL addAndSimplify (0, 0) $ map valuation [1..1000000]
  print $ maximum cancels

-----------

-- Returns the number written in base 5 with the least significant
-- digit first.
base5 :: Integral i => i -> [i]
base5 0 = []
base5 n = (n `mod` 5) : base5 (n `div` 5)

-- 5-adic valuation of n!
factorial5Adic :: Integral i => i -> i
factorial5Adic 0 = 0
factorial5Adic n = n `div` 5 + factorial5Adic (n `div` 5)

-- 2-adic valuation of n!
factorial2Adic :: Integral i => i -> i
factorial2Adic 0 = 0
factorial2Adic n = n `div` 2 + factorial5Adic (n `div` 2)

-- https://oeis.org/w/images/4/48/AlgLastFinal1.txt
lastDigitOfFactorial :: Integral i => i -> i
lastDigitOfFactorial n =
    let x = fromIntegral $ (factorial5Adic n) `mod` 4
        t = sum (filter even $ base5 n)
        z = (x + t `div` 2) `mod` 4 in
    if z == 0 then 6 else 2 ^ z

factorial :: Integral i => i -> i
factorial i = product [1..i]

lastDigitOfFactorial' :: Integral i => i -> i
lastDigitOfFactorial' n =
    let digits = base5 n in
    (6 * product [factorial a * 2 ^ (i * a) | i <- [0..] | a <- digits]) `mod` 10
