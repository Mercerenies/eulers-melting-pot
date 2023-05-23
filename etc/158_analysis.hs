
-- Problem 158 can be split into two parts. First, choose the letters
-- you're going to include. That's just choose(26, n). Second, choose
-- how you want to order these letters that satisfies the condition.
-- The problem is choose(26, n) * f(n), where f(n) is "number of ways
-- to order n things such that there's one increase". This file
-- calculates f(n) for small n so we can OEIS it.
--
-- Haha it's A000295! Eulerian numbers

import Data.List
import Control.Monad

choose :: Integral i => i -> i -> i
choose n k
    | 2 * k > n = choose n (n - k) -- Faster to do it this way
    | otherwise = product [(n-k+1)..n] `div` product [1..k]

adjacent :: [a] -> [(a, a)]
adjacent [] = []
adjacent [_] = []
adjacent (x:y:xs) = (x, y) : adjacent (y:xs)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

satisfiesCondition :: Ord a => [a] -> Bool
satisfiesCondition xs = (count (uncurry (<)) $ adjacent xs) == 1

f :: Int -> Int
f n = count satisfiesCondition (permutations [1..n])

-- https://en.wikipedia.org/wiki/Eulerian_number
f' :: Integral i => i -> i
f' n = (2 ^ n) - (n + 1)

p :: Integral i => i -> i
p n = (26 `choose` n) * f' n

main :: IO ()
main = print $ maximum [p n | n <- [1..26]]
