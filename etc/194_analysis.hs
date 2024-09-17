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
-- For this file, we're starting on the assumption that the four
-- corners are fixed to specific colors and we're asking the question
-- of how many ways to color the inside three vertices.

import qualified Data.List as List
import Control.Applicative

data CornerConfig = CornerConfig {
      topLeft :: FixedColor,
      topRight :: FixedColor,
      bottomLeft :: FixedColor,
      bottomRight :: FixedColor
    } deriving (Show, Eq)

data MiddleRow = MiddleRow Color Color Color
                 deriving (Show, Eq)

data FixedColor = Red | Green | Blue | Yellow
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Either a fixed, known color (from one of the corners) or an unknown
-- color that isn't used in any of the four corners.
data Color = FixedColor FixedColor | OtherColor
             deriving (Show, Eq)

factorial :: Integral i => i -> i
factorial n = product [1 .. n]

cornerColorSet :: CornerConfig -> [FixedColor]
cornerColorSet (CornerConfig {..}) = List.nub [ topLeft, topRight, bottomLeft, bottomRight ]

allColors :: [Color]
allColors = OtherColor : fmap FixedColor [minBound .. maxBound]

allMiddleRows :: [MiddleRow]
allMiddleRows = liftA3 MiddleRow allColors allColors allColors

middleRowToList :: MiddleRow -> [Color]
middleRowToList (MiddleRow a b c) = [a, b, c]

canTouch :: Color -> Color -> Bool
canTouch (FixedColor c) (FixedColor c') = c /= c'
canTouch _ _ = True

isValidMiddleRow :: MiddleRow -> Bool
isValidMiddleRow (MiddleRow a b c) =
    a `canTouch` b && b `canTouch` c

-- All possible color combinations for the central three cells.
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

-- Number of ways the given Unit can happen. Assumes the top-left and
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
            let distinctColors = fromIntegral . length $ cornerColorSet config in
            factorial (distinctColors - 2)

unitACount :: Integer -> Integer
unitACount = flip unitCount unitACorners

unitBCount :: Integer -> Integer
unitBCount = flip unitCount unitBCorners

main :: IO ()
main = do
  let totalColorCount = 4 -- Note: This must be >= 4, since we use four distinct colors in some configurations.
  putStrLn $ "Total for Unit A = " ++ show (unitACount totalColorCount)
  putStrLn $ "Total for Unit B = " ++ show (unitBCount totalColorCount)
