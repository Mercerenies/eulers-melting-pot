
-- Depends on: console, debug, effect, integers, lists, prelude, profunctor, rationals
--
-- Waaaay too slow :(

module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.List.Lazy (List(..), reverse, uncons, take, singleton, (:))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber, floor)
import Data.Number (sqrt)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Tuple (Tuple(..))
import Data.Lazy (Lazy, defer)
import Data.Foldable (any)
import Data.Unfoldable1 (range)
import Data.Function.Uncurried (Fn2, runFn2)
import Control.Alternative (guard)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

newtype CFrac i = CFrac (List i)

type Partition a = {
     prefix :: List a,
     suffix :: List a
}

derive instance Eq i => Eq (CFrac i)
derive instance Generic (CFrac i) _

instance Show i => Show (CFrac i) where
  show x = genericShow x

unCFrac :: forall i. CFrac i -> List i
unCFrac (CFrac xs) = xs

reversed :: forall i. CFrac i -> CFrac i
reversed = unCFrac >>> reverse >>> CFrac

instance Ord i => Ord (CFrac i) where
  compare (CFrac xs) (CFrac ys) = go (uncons xs) (uncons ys)
    where go Nothing Nothing = EQ
          go Nothing _ = GT
          go _ Nothing = LT
          go (Just xs) (Just ys) = compare xs.head ys.head <> go (uncons ys.tail) (uncons xs.tail)

intSqrt :: Int -> Int
intSqrt = toNumber >>> sqrt >>> floor

trunc :: forall i. Int -> CFrac i -> CFrac i
trunc n (CFrac xs) = CFrac $ take n xs

fromRatio :: forall i. Ord i => EuclideanRing i => Ratio i -> CFrac i
fromRatio r = CFrac $ go r
  where go r
         | denominator r == one = singleton $ numerator r
         | otherwise = let n = numerator r
                           d = denominator r in
                       (n / d) : go (d % (n `mod` d))

-- Returns list of { prefix, suffix }
partitions :: forall a. List a -> List (Partition a)
partitions xs = { prefix: mempty, suffix: xs } : rest
  where rest = case uncons xs of
                 Nothing -> mempty
                 Just xs' -> map (modifyPrefix (xs'.head :_)) $ partitions xs'.tail
        modifyPrefix f r = r { prefix = f r.prefix }

convergents :: forall i. CFrac i -> List (Tuple (CFrac i) (CFrac i))
convergents (CFrac xs) =
       let ps = partitions xs in
       map (\p -> Tuple (CFrac p.prefix) (CFrac p.suffix)) ps

commuteLazyList :: forall a. Lazy (List a) -> List a
commuteLazyList laz = List $ laz >>= \(List xs) -> xs

lazyCons :: forall a. a -> Lazy (List a) -> Lazy (List a)
lazyCons x = map (x :_)

infixr 6 lazyCons as :.

lazyAppend :: forall a. List a -> Lazy (List a) -> Lazy (List a)
lazyAppend xs ys = case uncons xs of
                     Just { head, tail } -> head :. defer (\_ -> commuteLazyList $ lazyAppend tail ys)
                     Nothing -> ys

infixr 5 lazyAppend as <>.

tailish :: forall a. List a -> List a
tailish xs = case uncons xs of
               Nothing -> xs
               Just { head: _, tail } -> tail

semiconvergents :: CFrac Int -> List (CFrac Int)
semiconvergents (CFrac xs) = map reversed $ go mempty xs
  where go vs xs = case uncons xs of
                     Nothing -> mempty
                     Just { head, tail } ->
                       let definiteSemiconvergents = map (\x' -> CFrac (x' : vs)) $ ((head + 2) `div` 2) `range` head in
                       commuteLazyList $ firstSemiconvergent vs head tail <>. definiteSemiconvergents <>. defer (\_ -> go (head : vs) tail)
        firstSemiconvergent vs x xs = do
              guard $ x `mod` 2 == 0
              guard $ CFrac (x : tailish vs) > CFrac (x : xs)
              pure $ CFrac ((x `div` 2) : vs)

realize :: Partial => CFrac Int -> Ratio Int
realize (CFrac xs) = go xs
  where go xs = case uncons xs of
                  Nothing -> crashWith "Empty CFrac in realize"
                  Just { head: x, tail: xs } ->
                    case uncons xs of
                      Nothing -> x % 1
                      Just _ -> x % 1 + one / go xs

reflect :: Ratio Int -> Ratio Int -> Ratio Int
reflect center a = (2 % 1) * center - a

sliding :: forall a. List a -> List (Tuple a a)
sliding xs = case uncons xs of
               Nothing -> mempty
               Just { head: x, tail: xs } ->
                 case uncons xs of
                   Nothing -> mempty
                   Just { head: x', tail: xs } ->
                     commuteLazyList $ Tuple x x' :. defer (\_ -> sliding (x' : xs))

isAmbiguous :: Partial => Ratio Int -> Boolean
isAmbiguous r =
  let sc = map realize $ semiconvergents (fromRatio r)
      adjacents = sliding sc in
  any (\(Tuple a b) -> denominator (reflect r a) < denominator b) adjacents

main :: Effect Unit
main = unsafePartial main'

main' :: Partial => Effect Unit
main' = do
  logShow $ countFractions unit
  logShow $ isAmbiguous (9 % 40)
  logShow $ isAmbiguous (8 % 40)
  log "🍝"

countFractions :: Partial => Unit -> Int
countFractions _ = runFn2 countFractionsJs (%) isAmbiguous

foreign import countFractionsJs :: Fn2 (Int -> Int -> Ratio Int) (Ratio Int -> Boolean) Int
