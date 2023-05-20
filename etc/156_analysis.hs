
import Data.List
import Control.Monad

-- Note: f((10^k)-1, d) = k(10^(k-1)), regardless of d, by basic
-- combinatorics (directly count the number of d digits that appear in
-- an arbitrary k-digit number). Since f is monotonic.

-- Side note for later: Holy cow, Arc (the official language we're
-- using here) actually outperforms Haskell by a factor of 3. Very
-- impressive. Haskell is ~30s, Arc is ~9s.

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

intToChar :: Int -> Char
intToChar x = toEnum (x + fromEnum '0')

-- Number of the digit d in the number n.
g :: Show i => i -> Int -> Int
g n d = count (\digit -> digit == intToChar d) (show n)

-- Number of the digit d up to the number n
f :: (Show i, Integral i) => i -> Int -> Int
f n d = sum [g n' d | n' <- [1..n]]

toNum :: Integral i => [i] -> i
toNum = go . reverse
    where go [] = 0
          go (n:ns) = n + 10 * go ns

fromNum :: Integral i => i -> [i]
fromNum = reverse . go
    where go 0 = []
          go n = let (n', d) = n `divMod` 10 in d : go n'

f' :: Integral i => [i] -> Int -> i
f' [] _ = 0
f' (n:ns) d =
    let k = fromIntegral (length ns)
        -- The lower blocks that are fully present. We can just
        -- multiply to get them by basic combinatorics.
        below = if k == 0 then 0 else n * k * 10 ^ (k - 1)
        -- If I'm currently the number we're looking for, count me for
        -- each value we'll hit. If I'm above the number, count a
        -- whole block for me.
        current = if n == fromIntegral d then
                      toNum ns + 1
                  else if n > fromIntegral d then
                      10 ^ k
                  else
                      0
        -- The stragglers at the end.
        recursive = f' ns d in
    below + current + recursive

upperLimit :: Integral i => i
upperLimit = 10 ^ 11

-- Simple binary search. Remember that f and id are monotonic, so if f
-- lower > upper or f upper < lower then there can't be any solutions
-- in [lower, upper).
findAllFixedPoints :: Integral i => i -> i -> Int -> i
findAllFixedPoints lower upper d
    | lower >= upper = 0
    | lower == upper - 1 = if f' (fromNum lower) d == lower then lower else 0
    | otherwise =
        let midpoint = (upper + lower) `div` 2
            fLower = f' (fromNum lower) d
            fUpper = f' (fromNum upper) d
            fMidpoint = f' (fromNum midpoint) d
            lowerSum = if fLower > midpoint || fMidpoint < lower then 0 else findAllFixedPoints lower midpoint d
            upperSum = if fMidpoint > upper || fUpper < midpoint then 0 else findAllFixedPoints midpoint upper d in
        lowerSum + upperSum

main :: IO ()
main = print (sum [findAllFixedPoints 0 upperLimit d | d <- [1..9]])
