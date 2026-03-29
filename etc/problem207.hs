
{-
 - Observations:
 -
 - We want 4^t = 2^t + k
 -
 - All quantities in question are positive, so we can
 - safely substitute t = log_2(w) (this is a bijection on
 - positive reals)
 -
 - Hence, w^2 - w = k, or w (w - 1) = k
 -
 - k is an integer. The problem statement additionally demands
 - that 2^t and 4^t are integers. If 2^t is an integer then
 - 4^t = (2^t)^2 is as well, so the second requirement is
 - superfluous. And 2^t is an integer exactly when t is a
 - base-2 logarithm of an integer, so the requirement that
 - 2^t is an integer is exactly the requirement that w is
 - an integer. Further, t is an integer if and only if w
 - is a power of 2.
 -}

-- 3 seconds in Haskell; solves

import Data.Ratio
import Control.Arrow

isPowerOf2 :: Integer -> Bool
isPowerOf2 n = go n 1
    where go n m = case n `compare` m of
                     GT -> go n (m * 2)
                     EQ -> True
                     LT -> False

allP :: [(Integer, Rational)]
allP = tail $ fmap (second safeToRatio) $ scanl go (0, (0, 0)) [2..]
    where go (_, (n, d)) w =
              let k = w * (w - 1)
                  n' = n + if isPowerOf2 w then 1 else 0
                  d' = d + 1 in
              (k, (n', d'))
          safeToRatio (_, 0) = 0
          safeToRatio (n, d) = n % d

findM :: Rational -> Integer
findM target = fst . head $ filter (\(_, pm) -> pm < target) allP

main :: IO ()
main = print $ findM (1 % 12345)
