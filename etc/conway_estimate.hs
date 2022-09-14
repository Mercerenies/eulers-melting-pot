{-# LANGUAGE RecordWildCards #-}

import Control.Arrow

data ConwayVec a = ConwayVec {
      front :: Vec a,
      left :: Vec a,
      right :: Vec a
    } deriving (Show)

type Vec a = (a, a)

add :: Num a => Vec a -> Vec a -> Vec a
add (x, y) (x', y') = (x + x', y + y')

sub :: Num a => Vec a -> Vec a -> Vec a
sub (x, y) (x', y') = (x - x', y - y')

start :: Num a => ConwayVec a
start = ConwayVec (1, 1) (1, 0) (0, 1)

moveLeft :: Num a => ConwayVec a -> ConwayVec a
moveLeft (ConwayVec { .. }) =
    ConwayVec {
      front = left `add` front,
      left = left,
      right = front
    }

moveRight :: Num a => ConwayVec a -> ConwayVec a
moveRight (ConwayVec { .. }) =
    ConwayVec {
      front = right `add` front,
      left = front,
      right = right
    }

moveBackLeft :: Num a => ConwayVec a -> ConwayVec a
moveBackLeft (ConwayVec { .. }) =
    ConwayVec {
      front = right,
      left = left,
      right = left `sub` right
    }

moveBackRight :: Num a => ConwayVec a -> ConwayVec a
moveBackRight (ConwayVec { .. }) =
    ConwayVec {
      front = left,
      left = left `sub` right,
      right = right
    }

nTimes :: Num a => Int -> (ConwayVec a -> ConwayVec a) -> (ConwayVec a -> ConwayVec a)
nTimes 0 _ = id
nTimes n f = f . nTimes (n - 1) f

fromStartTo44 :: Num a => ConwayVec a -> ConwayVec a
fromStartTo44 = nTimes 6 moveLeft

fromStartTo44' :: Num a => ConwayVec a -> ConwayVec a
fromStartTo44' = nTimes 2 moveLeft >>> nTimes 3 moveRight >>> moveLeft >>> nTimes 3 moveRight

iterateOnce :: Num a => ConwayVec a -> ConwayVec a
iterateOnce = nTimes 2 moveLeft >>> nTimes 4 moveRight >>> nTimes 2 moveLeft

main :: IO ()
main = do
  putStrLn "Solution 1"
  print (nTimes 6 moveLeft $ start)
  putStrLn "Solution 2"
  print ((nTimes 2 moveLeft >>> moveRight >>> moveLeft >>> moveRight) $ start)
  putStrLn "Solution 3"
  print ((nTimes 2 moveLeft >>> nTimes 2 moveRight >>> nTimes 2 moveLeft) $ start)
  putStrLn "Solution 4"
  print ((nTimes 2 moveLeft >>> nTimes 3 moveRight >>> moveLeft >>> nTimes 3 moveRight) $ start)

--  print (fromStartTo44 . iterateOnce $ start)
--  print (fromStartTo44 . iterateOnce . iterateOnce $ start)
--  print (fromStartTo44' . iterateOnce $ start)
--  print (fromStartTo44' . iterateOnce . iterateOnce $ start)

allMoves :: Int -> ConwayVec Int -> [ConwayVec Int]
allMoves 0 x = [x]
allMoves n x = do
  op <- [moveLeft >>> allMoves (n - 1), moveRight >>> allMoves (n - 1), pure]
  op x

allMovesBack :: Int -> ConwayVec Int -> [ConwayVec Int]
allMovesBack 0 x = [x]
allMovesBack n x = do
  op <- [moveBackLeft >>> allMoves (n - 1), moveBackRight >>> allMoves (n - 1), pure]
  op x

allMovesFromStart :: Int -> [Vec Int]
allMovesFromStart n = fmap front (allMoves n start ++ allMovesBack n start)

known :: Num a => [a]
known = [2, 5, 11, 21, 42, 152, 296, 1050, 2037, 7205, 13970, 49392, 95760, 338546, 656357, 2320437, 4498746]

mulSqrt5 :: Num a => Vec a -> Vec a -> Vec a
mulSqrt5 (x, y) (x', y') = (x * x' + 5 * y * y', x * y' + x' * y)

pellPrimitive :: Num a => Vec a
pellPrimitive = (9, 4)

nextPell :: Num a => Vec a -> Vec a
nextPell = mulSqrt5 pellPrimitive
