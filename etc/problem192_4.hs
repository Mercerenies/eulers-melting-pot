{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- Useful resources:
--
-- * https://math.stackexchange.com/a/4425617/84460
--
-- * https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
--
-- * https://en.wikipedia.org/wiki/Diophantine_approximation
--
-- * https://en.wikipedia.org/wiki/Continued_fraction#Semiconvergents

-- Works in 1 min 21 sec.

import Data.Ratio(Ratio, denominator)
import Data.Maybe(fromJust)
import Control.Monad
import Control.Arrow
import Debug.Trace

-- Note: The empty continued fraction CFrac [] represents the number
-- positive infinity. This is the only consistent way to handle the
-- ordering, with respect to our recursive definitions here.
newtype CFrac i = CFrac [i]
  deriving (Show, Eq)

-- As Data.List.find, but return the element BEFORE the match.
findBefore :: (a -> Bool) -> [a] -> Maybe a
findBefore _ [] = Nothing
findBefore _ [x] = Just x
findBefore p (x:y:xs)
    | p y = Just x
    | otherwise = findBefore p (y:xs)

unCFrac :: CFrac i -> [i]
unCFrac (CFrac xs) = xs

reversed :: CFrac i -> CFrac i
reversed = CFrac . reverse . unCFrac

instance Ord i => Ord (CFrac i) where
  compare (CFrac xs) (CFrac ys) = go xs ys
      where go [] [] = EQ
            go [] _  = GT
            go _ []  = LT
            go (x:xs) (y:ys) = compare x y <> go ys xs

intSqrt :: Integral i => i -> i
intSqrt n = floor (sqrt (fromIntegral n :: Double))

trunc :: Int -> CFrac i -> CFrac i
trunc n (CFrac xs) = CFrac $ take n xs

sqrtFrac :: Integral i => i -> CFrac i
sqrtFrac n = CFrac $ go 0 1 a0
    where a0 = intSqrt n
          go r s a =
              let r' = a * s - r
                  s' = (n - r' * r') `div` s
                  a' = (r' + a0) `div` s' in
              a : go r' s' a'

convergents :: CFrac i -> [(CFrac i, CFrac i)]
convergents (CFrac xs) = map (CFrac *** CFrac) . tail $ go xs
    where go [] = []
          go (x:xs) = ([], x:xs) : map (first (x:)) (go xs)

semiconvergents :: Integral i => CFrac i -> [CFrac i]
semiconvergents (CFrac xs) = map reversed $ go [] xs
    where go _ [] = []
          go vs (x:xs) =
              let definiteSemiconvergents = map (\x' -> CFrac (x' : vs)) [(x+2) `div` 2 .. x] in
              firstSemiconvergent vs x xs ++ definiteSemiconvergents ++ go (x : vs) xs
          firstSemiconvergent vs x xs = do
            guard $ x `mod` 2 == 0
            guard $ CFrac (x : tail vs) > CFrac (x : xs)
            pure $ CFrac ((x `div` 2) : vs)

semiconvergentsFor :: Integral i => (CFrac i, CFrac i) -> [CFrac i]
semiconvergentsFor (CFrac _, CFrac []) = error "semiconvergentsFor: CFrac []"
semiconvergentsFor (CFrac start, CFrac (x:end)) = firstSemiconvergent ++ definiteSemiconvergents
    where definiteSemiconvergents = map (\x' -> CFrac (start ++ [x'])) [(x+2) `div` 2 .. x]
          firstSemiconvergent = do
            guard $ x `mod` 2 == 0
            guard $ CFrac (x : reverse (tail start)) > CFrac (x : end)
            pure $ CFrac (start ++ [x `div` 2])

realize :: (Integral i, Fractional f) => CFrac i -> f
realize (CFrac xs) = go xs
    where go [] = error "realize: CFrac []"
          go [x] = fromIntegral x
          go (x:xs) = fromIntegral x + 1 / go xs

findBestSemiconvergent :: Integral i => i -> CFrac i -> Ratio i
findBestSemiconvergent limit = last . takeWhile (\r -> denominator r < limit) . map realize . semiconvergents

findBestFrac :: Integral i => i -> i -> Ratio i
findBestFrac limit = findBestSemiconvergent limit . sqrtFrac

findBestDenom :: Integral i => i -> i -> i
findBestDenom limit = denominator . findBestFrac limit

isPerfectSquare :: Integral i => i -> Bool
isPerfectSquare n = intSqrt n * intSqrt n == n

chop :: CFrac i -> CFrac i
chop (CFrac xs) = CFrac (init xs)

findBestSemiconvergentFast :: Integral i => i -> CFrac i -> Ratio i
findBestSemiconvergentFast limit frac =
    let convs = convergents frac
        (matchingConv, matchingTail) = fromJust $ findBefore (\(x, _) -> denominator (realize x) > limit) convs
        candidates = matchingConv : semiconvergentsFor (matchingConv, matchingTail) in
    fromJust $ findBefore (\x -> denominator x > limit) $ map realize candidates

findBestDenomFast :: Integral i => i -> i -> i
findBestDenomFast limit = denominator . findBestSemiconvergentFast limit . sqrtFrac

main :: IO ()
main = do
  let limit = 1_000_000_000_000
  --print $ findBestSemiconvergentFast 20 $ sqrtFrac 13
  --print $ findBestSemiconvergentFast 30 $ sqrtFrac 13
  --print $ sum [findBestDenom limit n | n <- [2 :: Integer .. 1_000], not (isPerfectSquare n)]
  print $ sum [findBestDenomFast limit n | n <- [2 :: Integer .. 100_000], not (isPerfectSquare n)]
