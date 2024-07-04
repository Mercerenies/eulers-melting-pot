{-# LANGUAGE ScopedTypeVariables #-}

-- Incomplete; was getting messy :)

import Data.List(sortOn)

data IsCorrect = Incorrect | Correct
                 deriving (Show, Read, Eq, Ord)

data GuessInfo = GuessInfo Char IsCorrect

data Guess = Guess {
      chars :: [Char],
      correctCount :: Int
    } deriving (Show, Read, Eq)

type Solution = [Char]

updateSeveral :: [(Int, a)] -> [a] -> [a]
updateSeveral updates elems = go (sortOn fst updates) (zip [0..] elems)
    where go _ [] = []
          go [] ys = map snd ys
          go ((n, x):xs) ((m, y):ys)
             | n == m = x : go xs ys
             | otherwise = y : go ((n, x):xs) ys

indices :: Guess -> [Int]
indices g = [0 .. length (chars g) - 1]

choose :: [a] -> Int -> [[a]]
choose _ 0 = pure []
choose [] _ = []
choose (x:xs) n = choose xs n ++ fmap (x :) (choose xs (n - 1))

digits :: [Char]
digits = "0123456789"

correctIndicesToBitmask :: Guess -> [Int] -> [IsCorrect]
correctIndicesToBitmask g idxs =
    let result = replicate (length $ chars g) Incorrect in
    updateSeveral (map (\i -> (i, Correct)) idxs) result

solveAll :: [Guess] -> [Solution]
solveAll guesses = go $ map (\g -> (g, indices g)) guesses
    where go :: [(Guess, [Int])] -> [Solution]
          go ((g, corrects) : xs) = do
            correctIndices <- corrects `choose` correctCount g
            let correctFlags = correctIndicesToBitmask g correctIndices
                correctInfo = zipWith GuessInfo (chars g) correctFlags
            undefined

smallGuesses :: [Guess]
smallGuesses = [
   Guess "90342" 2,
   Guess "70794" 0,
   Guess "39458" 2,
   Guess "34109" 1,
   Guess "51545" 2,
   Guess "12531" 1
  ]

main :: IO ()
main = pure ()
