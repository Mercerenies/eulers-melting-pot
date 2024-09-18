{-# LANGUAGE RecordWildCards #-}

-- We have a graph that looks like either
--
-- Unit A:
--
--  ?---?
--  |\ /|
--  | ? |
--  | | |
--  | ? |
--  | | |
--  | ? |
--  |/ \|
--  ?---?
--
-- or
--
-- Unit B:
--
--  ?---?
--  |\ /|
--  | ? |
--  | | |
--  | ? |
--  | | |
--  | ? |
--  |/ \|
--  ?   ?
--
-- For the start of this file, we're starting on the assumption that
-- the four corners are fixed to specific colors and we're asking the
-- question of how many ways to color the inside three vertices.
--
-- Attempt to find the general formula for `unitACount` and `unitBCount`.

import qualified Data.List as List
import Control.Applicative

zipLongestWith :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipLongestWith f a0 b0 as bs = go as bs
  where go (a:as) (b:bs) = f a b : go as bs
        go (a:as) [] = f a b0 : go as []
        go [] (b:bs) = f a0 b : go [] bs
        go [] [] = []

-- Polynomial, stored starting at the constant coefficient.
newtype Poly a = Poly [a]
  deriving Eq

instance Show a => Show (Poly a) where
  showsPrec n (Poly xs) = showParen (n > additionPrec) body
    where additionPrec = 6
          multiplicationPrec = 7
          body = foldr (.) id . List.intersperse (" + " ++) . fmap showTerm $ zip [0..] xs
          showTerm (0, x) = showsPrec additionPrec x
          showTerm (1, x) = showsPrec multiplicationPrec x . (" x" ++)
          showTerm (n, x) = showsPrec multiplicationPrec x . (" x^" ++) . shows n

unPoly :: Poly a -> [a]
unPoly (Poly xs) = xs

timesX :: Num a => Poly a -> Poly a
timesX (Poly xs) = Poly $ 0 : xs

varX :: Num a => Poly a
varX = Poly [0, 1]

evalPoly :: Num a => Poly a -> a -> a
evalPoly (Poly xs) n = go xs
  where go [] = 0
        go (x : xs) = x + n * go xs

instance Num a => Num (Poly a) where
  Poly xs + Poly ys = Poly $ zipLongestWith (+) 0 0 xs ys
  negate (Poly xs) = Poly $ fmap negate xs
  Poly [] * Poly _ = Poly []
  Poly (x:xs) * Poly ys = Poly (fmap (x *) ys) + timesX (Poly xs * Poly ys)
  abs (Poly xs) = Poly $ fmap abs xs
  signum (Poly xs) = Poly $ fmap signum xs
  fromInteger n = Poly [fromInteger n]

-- Four corners, consisting of fixed colors.
data CornerConfig = CornerConfig {
      topLeft :: FixedColor,
      topRight :: FixedColor,
      bottomLeft :: FixedColor,
      bottomRight :: FixedColor
    } deriving (Show, Eq)

-- The middle row, from top to bottom.
data MiddleRow = MiddleRow Color Color Color
                 deriving (Show, Eq)

-- A fixed color, coming from one of the four corners. Note that these
-- names are just stand-ins, and we'll eventually multiply by an
-- appropriate permutation (nPr) to handle the different possible
-- colors.
data FixedColor = Red | Green | Blue | Yellow
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Either a fixed, known color (from one of the corners) or an unknown
-- color that isn't used in any of the four corners. Unknown colors
-- are counted differently, since there are several possibilities for
-- them.
data Color = FixedColor FixedColor | OtherColor
             deriving (Show, Eq)

npr :: (Num a, Num b, Eq b) => a -> b -> a
npr _ 0 = 1
npr n r = n * npr (n - 1) (r - 1)

ncr :: Integral i => i -> i -> i
ncr n r = npr n r `div` factorial r

factorial :: Integral i => i -> i
factorial n = product [1..n]

cornerColorSet :: CornerConfig -> [FixedColor]
cornerColorSet (CornerConfig {..}) = List.nub [ topLeft, topRight, bottomLeft, bottomRight ]

allColors :: [Color]
allColors = OtherColor : fmap FixedColor [minBound .. maxBound]

allMiddleRows :: [MiddleRow]
allMiddleRows = liftA3 MiddleRow allColors allColors allColors

middleRowToList :: MiddleRow -> [Color]
middleRowToList (MiddleRow a b c) = [a, b, c]

-- Two of the same fixed color cannot touch, per the rules of graph
-- colorings. Two OtherColor constants can touch and will simply need
-- to be assigned (in `valuation`) different colors. If there aren't
-- enough colors, then `valuation` will return zero, effectively
-- dealing with the corner case of invalid adjacent OtherColors for
-- us.
canTouch :: Color -> Color -> Bool
canTouch (FixedColor c) (FixedColor c') = c /= c'
canTouch _ _ = True

isValidMiddleRow :: MiddleRow -> Bool
isValidMiddleRow (MiddleRow a b c) =
    a `canTouch` b && b `canTouch` c

-- All possible color combinations for the central three cells.
--
-- For a MiddleRow to be considered valid, all three of the following must be true:
--
-- 1. The row consists of only `OtherColor` and `FixedColor c`, where
-- `c` appears in the `corners` argument,
--
-- 2. Adjacent colors in the row satisfy `canTouch`, and
--
-- 3. The top color `canTouch` the top corners, and the bottom color
-- `canTouch` the bottom corners.
colorCombinations :: CornerConfig -> [MiddleRow]
colorCombinations corners =
      filter (\row -> usesOnlyValidColors row && isValidMiddleRow row && cornersAreValid row) allMiddleRows
    where cornerColors = cornerColorSet corners
          isValidColor OtherColor = True
          isValidColor (FixedColor c) = c `elem` cornerColors
          usesOnlyValidColors (MiddleRow a b c) =
              isValidColor a && isValidColor b && isValidColor c
          cornersAreValid (MiddleRow a _ c) =
              a `canTouch` FixedColor (topLeft corners) &&
              a `canTouch` FixedColor (topRight corners) &&
              c `canTouch` FixedColor (bottomRight corners) &&
              c `canTouch` FixedColor (bottomLeft corners)

-- The possible corner configurations for Unit A, up to renaming of
-- the corner colors.
unitACorners :: [CornerConfig]
unitACorners = [
  CornerConfig Red Yellow Green Blue, -- Four distinct colors
  CornerConfig Red Yellow Green Red, -- First diagonal matches
  CornerConfig Red Yellow Yellow Blue, -- Second diagonal matches
  CornerConfig Red Yellow Yellow Red -- Both diagonals match
 ]

-- The possible corner configurations for Unit B, up to renaming of
-- the corner colors. All Unit A configurations are valid for Unit B,
-- and so is one additional configuration.
unitBCorners :: [CornerConfig]
unitBCorners = additionalConfig : unitACorners
    where additionalConfig = CornerConfig Red Yellow Green Green

-- Assuming the corners are fixed, count the number of ways the given
-- middle row configuration can happen.
--
-- A `FixedColor` does not change the valuation (since the color is
-- already fixed). An `OtherColor` multiplies the valuation by the
-- total number of colors NOT present in the corners (possibly minus
-- one, if the previous color was also `OtherColor`).
valuation :: Num a => a -> CornerConfig -> MiddleRow -> a
valuation totalColorCount config row = go False $ middleRowToList row
    where cornerColorCount = fromIntegral . length $ cornerColorSet config
          -- The Boolean parameter indicates whether the last color we
          -- saw was OtherColor. Two OtherColors in a row restrict
          -- each other, so we have to take that into consideration.
          go _ [] = 1
          go _ (FixedColor _ : xs) = go False xs
          go lastWasOther (OtherColor : xs) =
              go True xs * (totalColorCount - cornerColorCount - if lastWasOther then 1 else 0)

-- Number of ways the given unit can happen. Assumes the top-left and
-- bottom-left corners are fixed (and distinct) but the top-right and
-- bottom-right can vary freely (since, when we glue a Unit onto an
-- existing graph, the leftmost two vertices are fixed but we decide
-- the rightmost two).
unitCount :: Num a => a -> [CornerConfig] -> a
unitCount totalColorCount configs = sum $ do
    config <- configs
    row <- colorCombinations config
    return $ valuation totalColorCount config row * colorPermutations config
  where colorPermutations config =
            -- The top-right and bottom-right colors MIGHT be new
            -- colors not present on the left. If they are, we need to
            -- consider all possible choices for those colors. This
            -- nPr calculation embodies those choices.
            let distinctColors = fromIntegral . length $ cornerColorSet config in
            (totalColorCount - 2) `npr` (distinctColors - 2)

unitACount :: Num a => Poly a
unitACount = unitCount varX unitACorners

unitBCount :: Num a => Poly a
unitBCount = unitCount varX unitBCorners

n :: Integer -> Integer -> Integer -> Integer
n a b c = ((a + b) `ncr` a) * c * (c - 1) * evalPoly unitACount c ^ a * evalPoly unitBCount c ^ b

main :: IO ()
main = do
  putStrLn $ "Unit A count (X = # of colors) = " ++ show unitACount
  putStrLn $ "Unit B count (X = # of colors) = " ++ show unitBCount
  print $ n 25 75 1984 `mod` 100000000

-- Notes on FCaMP program::
--
-- Cell values:
--
-- - (0, 2) - Constant 10^8
--
-- - (1, 2) - Constant 1984
--
-- Functions:
--
-- - (0, 3) - Power function
--
-- Other notes:
--
-- - R consumes a stack but C doesn't create one, so we have to make
--   our own call stacks for each function call (with '[').
--
-- - When I need to do the (100 nCr 25) calculation, I manually
--   eliminate the denominator on paper and end up left with 13 terms
--   to multiply in the numerator. This way, we can still do
--   everything modulo 10^8 and not exceed 64-bit integers.
--
-- - "2t" everywhere converts to 64-bit integers. The default for
-- - literals is a _byte_. Yes, a _byte_.
