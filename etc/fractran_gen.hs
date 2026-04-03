
-- New FRACTRAN generator, for Problem 207

import Data.List hiding (singleton)
import Data.Function
import Control.Monad.RWS

-- Number stored as a pair of (prime index, power), in ascending order
-- of prime index.
data Number = Number [(Int, Integer)]
              deriving (Eq)

data Fraction = Fraction Number Number
                deriving (Eq)

data FractranState = FractranState {
      stateNextIndex :: Int,
      stateLastLabel :: Label
    } deriving (Show)

number :: [(Int, Integer)] -> Number
number xs = Number xs'
    where xs' = filter (\(_, p) -> p /= 0) . map collect . groupBy ((==) `on` fst) $ sort xs
          collect xs = (fst (head xs), sum (map snd xs))

singleton :: Int -> Number
singleton i = Number [(i, 1)]

instance Semigroup Number where
    Number xs <> Number ys = number (xs ++ ys)

instance Monoid Number where
    mempty = Number []

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

(%%) :: Number -> Number -> Fraction
(%%) = fraction

infix 5 %%

newtype Var = Var Int
    deriving (Show, Eq)

newtype Label = Label Int
    deriving (Show, Eq)

getVar :: Var -> Int
getVar (Var i) = i

getLabel :: Label -> Int
getLabel (Label i) = i

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
             stateNextIndex = 2,
             stateLastLabel = startLabel
           }

-- It is assumed that the program starts with the minimal input [2, 1].
startLabel :: Label
startLabel = Label 0

-- Reserve the output variable in advance, so we know where to look
-- for the final result.
outputVar :: Var
outputVar = Var 1

rawFraction :: Fraction -> Fractran ()
rawFraction r = Fractran $ tell [r]

printFractran :: Fractran () -> IO ()
printFractran program = let ((), _, out) = runRWS (runFractran program) () newState in
                        mapM_ print out

rawReserveValue :: Fractran Int
rawReserveValue = Fractran $ do
                    value <- gets stateNextIndex
                    modify $ \st -> st { stateNextIndex = stateNextIndex st + 1 }
                    return value

reserveVar :: Fractran Var
reserveVar = fmap Var rawReserveValue

reserveLabel :: Fractran Label
reserveLabel = fmap Label rawReserveValue

getLastLabel :: Fractran Label
getLastLabel = Fractran $ gets stateLastLabel

putLastLabel :: Label -> Fractran ()
putLastLabel l = Fractran . modify $ \st -> st { stateLastLabel = l }

-- Execute a command. That command must assume that the first label
-- starts at value 1 and must set the second to value 1 when it's
-- done.
--
-- The command must not use the first label nontrivially, but it may
-- use the second label as part of its control flow. This asymmetry is
-- necessary since if commands could use BOTH labels, then multiple
-- adjacent commands could clobber each other without knowing it.
atomicCommand :: (Label -> Label -> Fractran ()) -> Fractran ()
atomicCommand f = do
  lastLabel <- getLastLabel
  nextLabel <- reserveLabel
  f lastLabel nextLabel
  putLastLabel nextLabel

-- Run at the end of the program once to clear the final label, just
-- to clean up the output.
clearFinalLabel :: Fractran ()
clearFinalLabel = do
  Label lastLabel <- Fractran $ gets stateLastLabel
  rawFraction $ mempty %% singleton lastLabel

-- a += N c; b += N c; c = 0;
drainAndAddWithTwo :: Var -> Var -> Var -> Integer -> Fractran ()
drainAndAddWithTwo (Var a) (Var b) (Var c) n =
    atomicCommand $ \lastLabel nextLabel -> do
      tmp <- rawReserveValue
      rawFraction $ number [(a, n), (b, n), (tmp, 1)] %% singleton c <> singleton (getLabel lastLabel)
      rawFraction $ singleton (getLabel lastLabel) %% singleton tmp
      rawFraction $ singleton (getLabel nextLabel) %% singleton (getLabel lastLabel)

-- a += N b; b = 0;
drainAndAddWith :: Var -> Var -> Integer -> Fractran ()
drainAndAddWith (Var a) (Var b) n = atomicCommand $ \lastLabel nextLabel -> do
                                      tmp <- rawReserveValue
                                      rawFraction $ number [(a, n), (tmp, 1)] %% singleton b <> singleton (getLabel lastLabel)
                                      rawFraction $ singleton (getLabel lastLabel) %% singleton tmp
                                      rawFraction $ singleton (getLabel nextLabel) %% singleton (getLabel lastLabel)

-- a += b; b = 0;
drainAndAdd :: Var -> Var -> Fractran ()
drainAndAdd a b = drainAndAddWith a b 1

-- a += b*c; b = 0
-- Mostly copied from the Esolang page example.
drainAndMultiply :: Var -> Var -> Var -> Fractran ()
drainAndMultiply (Var a) (Var b) (Var c) = atomicCommand $ \(Label lastLabel) (Label nextLabel) -> do
                                             tmp13 <- rawReserveValue
                                             tmp11 <- rawReserveValue
                                             tmp7 <- rawReserveValue
                                             rawFraction $ singleton tmp13 %% singleton tmp7 <> singleton c
                                             rawFraction $ singleton a <> singleton tmp7 <> singleton tmp11 %% singleton tmp13
                                             rawFraction $ singleton lastLabel %% singleton tmp7
                                             rawFraction $ singleton c %% singleton tmp11
                                             rawFraction $ singleton tmp7 %% singleton b <> singleton lastLabel
                                             rawFraction $ singleton nextLabel %% singleton lastLabel

-- a = 0;
zeroOut :: Var -> Fractran ()
zeroOut (Var a) = atomicCommand $ \lastLabel nextLabel -> do
                    tmp <- rawReserveValue
                    rawFraction $ singleton tmp %% singleton a <> singleton (getLabel lastLabel)
                    rawFraction $ singleton (getLabel lastLabel) %% singleton tmp
                    rawFraction $ singleton (getLabel nextLabel) %% singleton (getLabel lastLabel)

-- a += CONSTANT
addConst :: Var -> Integer -> Fractran ()
addConst (Var a) value = atomicCommand $ \lastLabel nextLabel -> do
                           rawFraction $ number [(a, value), (getLabel nextLabel, 1)] %% singleton (getLabel lastLabel)

-- a = CONSTANT
putConst :: Var -> Integer -> Fractran ()
putConst a value = zeroOut a >> addConst a value

-- a -= 1 (saturating sub)
satSub1 :: Var -> Fractran ()
satSub1 (Var a) = atomicCommand $ \(Label lastLabel) (Label nextLabel) -> do
                    rawFraction $ singleton nextLabel %% singleton a <> singleton lastLabel
                    rawFraction $ singleton nextLabel %% singleton lastLabel

-- a = max(a-b, 0); b = max(b-a, 0);
symmDiff :: Var -> Var -> Fractran ()
symmDiff (Var a) (Var b) =
    atomicCommand $ \(Label lastLabel) (Label nextLabel) -> do
      tmp <- rawReserveValue
      rawFraction $ singleton tmp %% singleton a <> singleton b <> singleton lastLabel
      rawFraction $ singleton lastLabel %% singleton tmp
      rawFraction $ singleton nextLabel %% singleton lastLabel

-- while (a > 0) { a -= 1; body ... }
whilePositive :: Var -> Fractran () -> Fractran ()
whilePositive (Var a) body =
    atomicCommand $ \(Label lastLabel) (Label nextLabel) -> do
      Label conditionCheckLabel <- reserveLabel
      Label innerLoopLabel <- reserveLabel
      rawFraction $ singleton conditionCheckLabel %% singleton lastLabel
      rawFraction $ singleton innerLoopLabel %% singleton conditionCheckLabel <> singleton a
      rawFraction $ singleton nextLabel %% singleton conditionCheckLabel
      putLastLabel (Label innerLoopLabel)
      body
      Label bodyEndLabel <- getLastLabel
      rawFraction $ singleton conditionCheckLabel %% singleton bodyEndLabel

-- a = b;
mov :: Var -> Var -> Fractran ()
mov a b = do
  tmp <- reserveVar
  zeroOut a
  zeroOut tmp
  drainAndAdd tmp b
  drainAndAddWithTwo a b tmp 1

-- Because the interpreter insists on fully evaluating the program
-- constants and then factoring them, we can't just write p^12345 and
-- expect it to work. So we have to get clever and produce this number
-- some other way.
produce12345 :: Var -> Fractran ()
produce12345 outVar = do
  zeroOut outVar
  tmp1 <- reserveVar
  tmp2 <- reserveVar
  putConst tmp1 123
  putConst tmp2 100
  drainAndMultiply outVar tmp1 tmp2
  addConst outVar 45
  zeroOut tmp2

problem207 :: Fractran ()
problem207 = do
  n <- reserveVar
  twoN <- reserveVar
  left <- reserveVar
  right <- reserveVar
  constant <- reserveVar

  putConst n 1
  putConst twoN 2

  produce12345 constant
  zeroOut left
  drainAndMultiply left constant n
  mov right twoN
  satSub1 right
  symmDiff left right
  whilePositive left $ do
    addConst left 1 -- Counteract the implicit 'a--' of the while loop
    addConst n 1
    mov left twoN
    drainAndAdd twoN left

    produce12345 constant
    zeroOut left
    drainAndMultiply left constant n
    mov right twoN
    satSub1 right
    symmDiff left right

  --zeroOut twoN
  --zeroOut right

  --satSub1 n
  --produce12345 constant
  --drainAndMultiply left constant n -- w(left) = 12345 * (n - 1)
  --addConst left 2
  --mov constant left -- constant = w - 1
  --drainAndMultiply outputVar constant left

  clearFinalLabel

sampleProgram :: Fractran ()
sampleProgram = do
  a <- reserveVar
  b <- reserveVar
  putConst b 9
  mov a b
  clearFinalLabel

main :: IO ()
main = do
  putStrLn $ "Input: [2, 1]"
  printFractran problem207
  --printFractran sampleProgram
