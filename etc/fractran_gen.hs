
-- New FRACTRAN generator, for Problem 207

import Data.List
import Data.Function
import Control.Monad.RWS

-- Number stored as a pair of (prime index, power), in ascending order
-- of prime index.
data Number = Number [(Int, Integer)]
              deriving (Eq)

data Fraction = Fraction Number Number
                deriving (Eq)

data FractranState = FractranState {
      stateNextIndex :: Int
    } deriving (Show)

number :: [(Int, Integer)] -> Number
number xs = Number xs'
    where xs' = filter (\(_, p) -> p /= 0) . map collect . groupBy ((==) `on` fst) $ sort xs
          collect xs = (fst (head xs), sum (map snd xs))

newtype Fractran a = Fractran { runFractran :: RWS () [Fraction] FractranState a }
    deriving (Functor, Applicative, Monad)

-- Takes two sorted lists and runs the second half of mergesort on
-- them.
mergeBy :: (a -> b -> Ordering) -> (a -> b -> (a, b)) -> [a] -> [b] -> ([a], [b])
mergeBy cmp mergeFn as bs = go as bs
    where go [] bs = ([], bs)
          go as [] = (as, [])
          go (a:as) (b:bs) =
              case cmp a b of
                LT -> let (as', bs') = go as (b:bs) in
                      (a:as', bs')
                GT -> let (as', bs') = go (a:as) bs in
                      (as', b:bs')
                EQ -> let (a', b') = mergeFn a b
                          (as', bs') = go as bs in
                      (a':as', b':bs')

fraction :: Number -> Number -> Fraction
fraction n d = let (n', d') = normalForm n d in
               Fraction n' d'
    where normalForm (Number xs) (Number ys) =
              let (xs', ys') = mergeBy (compare `on` fst) (\(xi, xp) (_, yp) -> ((xi, max (xp - yp) 0), (xi, max (yp - xp) 0))) xs ys in
              (number xs', number ys')

newtype Var = Var Int
    deriving (Show, Eq)

instance Show Number where
    showsPrec _ (Number []) = ("1" ++)
    showsPrec _ (Number xs) = foldr (.) id . intersperse ("*" ++) $ map (\(index, power) -> shows (allPrimes !! index) . ("^" ++) . shows power) xs

instance Show Fraction where
    showsPrec _ (Fraction a b) = shows a . (" % " ++) . shows b

allPrimes :: [Integer]
allPrimes = filter isPrime [2..]
    where isPrime n = not . any (`divides` n) $ takeWhile (\i -> i * i <= n) [2..]
          divides a b = b `mod` a == 0

newState :: FractranState
newState = FractranState {
             stateNextIndex = 0
           }

rawFraction :: Fraction -> Fractran ()
rawFraction r = Fractran $ tell [r]

printFractran :: Fractran () -> IO ()
printFractran program = let ((), _, out) = runRWS (runFractran program) () newState in
                        mapM_ print out

reserveVar :: Fractran Var
reserveVar = Fractran $ do
               varValue <- gets stateNextIndex
               modify $ \st -> st { stateNextIndex = stateNextIndex st + 1 }
               return $ Var varValue

sampleProgram :: Fractran ()
sampleProgram = do
  rawFraction (Fraction (Number [(0, 3), (2, 4)]) (Number [(4, 2), (5, 3)]))
  rawFraction (Fraction (Number [(1, 3), (2, 4)]) (Number [(4, 2), (5, 3)]))

main :: IO ()
main = do
  printFractran sampleProgram
  print $ fraction (number [(1, 2), (3, 5), (1, 1)]) (number [(1, 1)])
