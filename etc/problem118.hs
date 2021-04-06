{-# LANGUAGE ScopedTypeVariables #-}

-- After 11 minutes, this still doesn't bear fruit. Brute forcing the
-- solution is infeasible.

import Data.List(foldl')
import Control.Monad(guard)

digits :: [Int]
digits = [1..9]

allPartitions :: forall a. [a] -> [[[a]]]
allPartitions = go []
    where go :: [[a]] -> [a] -> [[[a]]]
          go acc [] = pure acc
          go acc (x:xs) = do
            acc' <- place acc x
            go acc' xs
          place :: [[a]] -> a -> [[[a]]]
          place [] x = pure [[x]]
          place (ys:yss) x = ((x:ys) : yss) : fmap (ys :) (place yss x)

allOrders :: forall a. [a] -> [[a]]
allOrders = go
    where go :: [a] -> [[a]]
          go [] = pure []
          go (x:xs) = do
            rest <- go xs
            place x rest
          place :: a -> [a] -> [[a]]
          place x [] = pure [x]
          place x (y:ys) = (x:y:ys) : fmap (y :) (place x ys)

isPrime :: Integral i => i -> Bool
isPrime x = x >= 2 && all (\i -> x `mod` i /= 0) [2 .. x `div` 2]

numberOf :: Num a => [a] -> a
numberOf = foldl' (\x y -> x * 10 + y) 0

solutions :: [[Int]]
solutions = do
  candidate <- allPartitions digits >>= mapM allOrders
  let candidate' = fmap numberOf candidate
  guard $ all isPrime candidate'
  return candidate'

main :: IO ()
main = print (length solutions)
