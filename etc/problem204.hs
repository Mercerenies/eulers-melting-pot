
-- 25 seconds

isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2..n `div` 2], n `mod` x == 0]

primesUpTo :: Integer -> [Integer]
primesUpTo n = filter isPrime [2..n]

enumerateHammingNumbers :: [Integer] -> Integer -> Integer -> Integer
enumerateHammingNumbers [] acc limit = if acc <= limit then 1 else 0
enumerateHammingNumbers (p:ps) acc limit
    | acc > limit = 0
    | otherwise = enumerateHammingNumbers ps acc limit + enumerateHammingNumbers (p:ps) (acc * p) limit

main :: IO ()
main = do
  let k = 100
  let limit = 10 ^ (9 :: Int)
  let relevantPrimes = primesUpTo k
  print $ enumerateHammingNumbers relevantPrimes 1 limit
