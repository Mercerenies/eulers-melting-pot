
-- https://faculty.evansville.edu/ck6/encyclopedia/ETC.html

a, b, c :: Double
a = 399
b = 455
c = 511

desiredResult :: Double
desiredResult = 784

{-
cosθ :: Double
cosθ = (a ^ 2 + b ^ 2 - c ^ 2) / (2 * a * b)

sinθ :: Double
sinθ = sqrt (4 * a ^ 2 * b ^ 2 - (a ^ 2 + b ^ 2 - c ^ 2) ^ 2) / (2 * a * b)

x² :: Double
x² = a ^ 2 + b ^ 2 - 2 * a * b * (cosθ / 2 - sqrt 3 * sinθ / 2)

x :: Double
x = sqrt x²
-}

x = sqrt x²
    where x² = 0.5 * (a ^ 2 + b ^ 2 + c ^ 2 + sqrt δ)
          δ = 6 * (a ^ 2 * b ^ 2 + a ^ 2 * c ^ 2 + b ^ 2 * c ^ 2) - 3 * (a ^ 4 + b ^ 4 + c ^ 4)

main :: IO ()
main = print x
