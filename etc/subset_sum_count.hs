
-- Just a bit of background work for #106.

factorial :: Integral a => a -> a
factorial k = product [1..k]

choose :: Integral a => a -> a -> a
choose n k = factorial n `div` (factorial k * factorial (n - k))

possibleSubsetPairs :: Integral a => a -> a
possibleSubsetPairs n = sum [(n `choose` i) * (2 ^ (n - i) - 1) | i <- [1..n-1]] `div` 2
