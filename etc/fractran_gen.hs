
-- New FRACTRAN generator, for Problem 207

import Data.List
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

newtype Fractran a = Fractran { runFractran :: RWS () [Fraction] FractranState a }
    deriving (Functor, Applicative, Monad)

newtype Var = Var Int
    deriving (Show, Eq)

instance Show Number where
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
main = printFractran sampleProgram

