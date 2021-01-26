
import Data.Ratio

-- For Euler 113; just simplifying some polynomial formulas

data Poly a = Poly [a]
              deriving (Show, Read)

pad :: Int -> a -> [a] -> [a]
pad n x xs = xs ++ replicate extra x
    where extra = n - length (take n xs)

factorial :: Integral a => a -> a
factorial n = product [1..n]

nCr :: Integral a => a -> a -> a
nCr n r = factorial n `div` (factorial r * factorial (n - r))

bernoulli :: Integral a => a -> Ratio a
bernoulli m = sum [sum [term v k | v <- [0..k]] | k <- [0..m]]
    where term v k = (-1) ^ v * (k `nCr` v) * (v + 1) ^ m % (k + 1)

timesX :: Num a => Poly a -> Poly a
timesX (Poly xs) = Poly (0 : xs)

instance Num a => Num (Poly a) where
    Poly xs + Poly ys =
        let xs' = pad (length ys) 0 xs
            ys' = pad (length xs) 0 ys in
        Poly $ zipWith (+) xs' ys'
    Poly [] * Poly _ = Poly []
    Poly (x : xs) * Poly ys =
        Poly (fmap (x *) ys) + timesX (Poly xs * Poly ys)
    negate (Poly xs) = Poly (fmap negate xs)
    abs (Poly xs) = Poly (fmap abs xs)
    signum (Poly xs) = Poly (fmap signum xs)
    fromInteger n = Poly [fromInteger n]

constant :: a -> Poly a
constant x = Poly [x]

infixl 7 .*

(.*) :: Num a => a -> Poly a -> Poly a
x .* p = constant x * p

eval :: Num a => Poly a -> a -> a
eval (Poly []) _ = 0
eval (Poly (x : xs)) n = x + n * eval (Poly xs) n

-- Converts sum [ k ^ p | k <- [1..n] ] to a polynomial
powerSum :: Integral a => a -> Poly (Ratio a)
powerSum p = (1 % (p + 1)) .* Poly [term i | i <- [0..p+1]]
    where term 0 = 0
          term i = fromIntegral ((p + 1) `nCr` (p + 1 - i)) * bernoulli (p + 1 - i)

polySum :: Integral a => Poly (Ratio a) -> Poly (Ratio a)
polySum (Poly xs) = sum $ zipWith (\p x -> x .* powerSum p) [0..] xs

increasingNumbers :: Integral a => Poly (Ratio a)
increasingNumbers = foldr (.) id (replicate 9 polySum) $ constant 1

decreasingNumbers :: Integral a => Poly (Ratio a)
decreasingNumbers = (foldr(.) id (replicate 10 polySum) $ constant 1) + Poly [1, -1]

doubleCounted :: Integral a => a -> a
doubleCounted p = 1 + 9 * p

solve :: Integer -> Rational
solve n = eval (increasingNumbers + decreasingNumbers) ((n + 1) % 1) - (doubleCounted n % 1) - 1
-- The last -1 is simply because the challenge isn't counting zero and my formula does.
