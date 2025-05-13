
-- 25 seconds

import Data.Function
import Control.Monad

isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2..n `div` 2], n `mod` x == 0]

primesUpTo :: Integer -> [Integer]
primesUpTo n = filter isPrime [2..n]

powersToNum :: [(Integer, Integer)] -> Integer
powersToNum = product . map (uncurry (^))

enumerateHammingNumbers :: [Integer] -> Integer -> Integer -> [Integer]
enumerateHammingNumbers [] acc limit = acc <$ guard (acc <= limit)
enumerateHammingNumbers (p:ps) acc limit
    | acc > limit = []
    | otherwise = enumerateHammingNumbers ps acc limit ++ enumerateHammingNumbers (p:ps) (acc * p) limit

main :: IO ()
main = do
  let k = 100
  let limit = 10 ^ 9
  let relevantPrimes = primesUpTo k
  print . length $ enumerateHammingNumbers relevantPrimes 1 limit
