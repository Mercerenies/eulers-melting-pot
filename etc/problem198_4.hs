
-- Quick test using the same approach as problem198_3.cpp but with
-- direct recursion. To see if we can use functional languages.
--
-- Not even close. Takes 8 seconds to run for 10e6, and we need 10e8.

data Fraction = Fraction {
      numer :: Integer,
      denom :: Integer
    } deriving (Show, Read, Eq, Ord)

denominatorBound :: Integer
denominatorBound = 100000000

rangeBound :: Integer
rangeBound = 100

countAmbiguousNumbers :: Fraction -> Fraction -> Integer
countAmbiguousNumbers lower upper =
    let avg = Fraction (numer lower * denom upper + numer upper * denom lower) (denom lower * denom upper * 2) in
    if denom avg > denominatorBound then
        0
    else
        let mediant = Fraction (numer lower + numer upper) (denom lower + denom upper)
            thisCount = if rangeBound * numer avg < denom avg then 1 else 0
            recursiveCount = if denom mediant > denominatorBound then
                                 0
                             else
                                 countAmbiguousNumbers lower mediant + countAmbiguousNumbers mediant upper in
        thisCount + recursiveCount

main :: IO ()
main = do
  print $ countAmbiguousNumbers (Fraction 0 1) (Fraction 1 50)
