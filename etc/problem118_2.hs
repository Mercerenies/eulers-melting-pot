{-# LANGUAGE ScopedTypeVariables #-}

-- Here we go again.

-- Noooooope

import Data.List(foldl', sort)
import Control.Monad(guard, filterM)
import Data.Map(Map, (!))
import qualified Data.Map as Map

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

allSubsets :: [a] -> [[a]]
allSubsets = filterM (const [False, True])

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

primeCounts :: Map [Int] Int
primeCounts = Map.fromList [(sort dgs, countPrimePerms dgs) | dgs <- allSubsets digits]
    where countPrimePerms = length . filter (isPrime . numberOf) . allOrders

solutions :: Int
solutions = sum $ do
  let allCounts = primeCounts
  candidate <- allPartitions digits
  let count = product $ fmap (\c -> allCounts ! sort c) candidate
  return count

main :: IO ()
main = print solutions
