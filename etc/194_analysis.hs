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
-- This solves, instantaneously.

import qualified Data.List as List
import Control.Applicative

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

npr :: Integral i => i -> i -> i
npr n r = factorial n `div` factorial (n - r)

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
valuation :: Integer -> CornerConfig -> MiddleRow -> Integer
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
unitCount :: Integer -> [CornerConfig] -> Integer
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

unitACount :: Integer -> Integer
unitACount = flip unitCount unitACorners

unitBCount :: Integer -> Integer
unitBCount = flip unitCount unitBCorners

n :: Integer -> Integer -> Integer -> Integer
n a b c = ((a + b) `ncr` a) * c * (c - 1) * unitACount c ^ a * unitBCount c ^ b

{-
-- Original main function, used for testing.
main :: IO ()
main = do
  let totalColorCount = 4 -- Note: This must be >= 4, since we use four distinct colors in some configurations.
  putStrLn $ "Total for Unit A = " ++ show (unitACount totalColorCount)
  putStrLn $ "Total for Unit B = " ++ show (unitBCount totalColorCount)
-}

main :: IO ()
main = print $ n 25 75 1984 `mod` 100000000
