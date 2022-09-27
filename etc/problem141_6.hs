
-- Copy of problem141_4.c, or (indirectly) problem141_3.raku. Testing
-- a thing to see if we can do this functional-like.

import Data.List
import Control.Monad
import Debug.Trace

limit :: Integral i => i
limit = 1000000000000

aValues :: Integral i => [i]
aValues = [1..10000]

bValues :: Integral i => i -> [i]
bValues a = takeWhile (\b -> b * a * a * a + b * b < limit) [1..a-1]

cValues :: Integral i => i -> i -> [i]
cValues a b = takeWhile (\c -> nValue a b c < limit) [1..]

nValue :: Integral i => i -> i -> i -> i
nValue a b c = c * c * b * a * a * a + c * b * b

isSquare :: Integral i => i -> Bool
isSquare n = sqrt n' == fromIntegral (floor (sqrt n'))
    where n' :: Double
          n' = fromIntegral n

main :: IO ()
main = print . sum . nub $ allNumbers
    where allNumbers = do
            a <- aValues
            b <- bValues a
            guard $ gcd a b == 1
            c <- cValues a b
            let n = nValue a b c
            guard $ isSquare n
            return n
