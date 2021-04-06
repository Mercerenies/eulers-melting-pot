{-# LANGUAGE ScopedTypeVariables #-}

-- Okay, this time let's try it with less naive brute forcing and more intelligent brute forcing

-- Also too slow... ... ... (after 12 minutes no answer)

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

solutions :: Int
solutions = sum $ do
  candidate <- allPartitions digits
  let count = product $ fmap (length . filter (isPrime . numberOf) . allOrders) candidate
  return count

main :: IO ()
main = print solutions
