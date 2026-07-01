
-- Ended up not being used, see problem209.py instead.

import Data.Char

-- An endomorphism on a finite set.
newtype FinEndo = FinEndo [Int]
    deriving (Show, Eq, Ord)

appFinEndo :: FinEndo -> Int -> Int
appFinEndo (FinEndo xs) n = xs !! n

composeFinEndo :: FinEndo -> FinEndo -> FinEndo
composeFinEndo (FinEndo xs) (FinEndo ys) = FinEndo [appFinEndo (FinEndo ys) i | i <- xs]

(.$) :: FinEndo -> Int -> Int
(.$) = appFinEndo

infixr 0 .$

identity :: Int -> FinEndo
identity n = FinEndo [i | i <- [0..n - 1]]
