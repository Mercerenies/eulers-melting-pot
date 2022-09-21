
import Data.Ratio

ϕ :: Floating a => a
ϕ = (1 + sqrt 5) / 2

ψ :: Floating a => a
ψ = (1 - sqrt 5) / 2

a :: (Integral i, Floating a) => i -> a
a i = ((1 - 3 * ψ) * (ϕ ^^ i) + (3 * ϕ - 1) * (ψ ^^ i)) / (ϕ - ψ)

--main :: IO ()
--main = print [round (a n) | n <- [0..15]]

-------------------------- (Redoing 137 work)

discriminant :: Num a => a -> a
discriminant k = 5 * k * k + 2 * k + 1

isPerfectSquare :: Double -> Bool
isPerfectSquare x = sqrt x == fromInteger (floor (sqrt x))

xValue :: Integral a => a -> Ratio a
xValue k = ((- k - 1) + round (sqrt (discriminant k'))) % (2 * k)
    where k' = fromIntegral k :: Double

xValue' :: Integral a => a -> Ratio a
xValue' k = ((- k - 1) - round (sqrt (discriminant k'))) % (2 * k)
    where k' = fromIntegral k :: Double

--main :: IO ()
--main = do
--  let ks = take 5 [round k | k <- [1..], isPerfectSquare (discriminant k)]
--  print ks
--  print (map xValue ks)
--  print (map xValue' ks)

-------------------------- (Now for 140)

discriminantG :: Num a => a -> a
discriminantG k = 5 * k * k + 14 * k + 1

xValueG :: Integral a => a -> Ratio a
xValueG k = ((- k - 1) + round (sqrt (discriminantG k'))) % (2 * (k + 3))
    where k' = fromIntegral k :: Double

xValueG' :: Integral a => a -> Ratio a
xValueG' k = ((- k - 1) - round (sqrt (discriminantG k'))) % (2 * (k + 3))
    where k' = fromIntegral k :: Double

main :: IO ()
main = do
  let ks = take 10 [round k | k <- [1..], isPerfectSquare (discriminantG k)]
  print ks
  print (map xValueG ks)
  print (map xValueG' ks)
