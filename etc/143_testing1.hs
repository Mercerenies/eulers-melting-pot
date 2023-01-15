
π :: Double
π = pi

a, b, c :: Double
a = 399
b = 455
c = 511

θ :: Double -> Double -> Double -> Double
θ a b c = acos ((b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c))

ϕ :: Double -> Double -> Double -> Double
ϕ a b c = θ a b c + π / 3

a', b', c' :: Double -> Double
a' a = a / sqrt 3
b' b = b / sqrt 3
c' c = c / sqrt 3

z :: Double -> Double -> Double -> Double
z a b c = sqrt (b' b ^ 2 + c' c ^ 2 - 2 * b' b * c' c * cos (ϕ a b c))

ψ :: Double -> Double -> Double -> Double
ψ a b c = asin ((c' c * sin (ϕ a b c)) / z a b c)

p :: Double -> Double -> Double -> Double
p a b c = sqrt (2 * b' b ^ 2 * (1 - cos (2 * ψ a b c)))

sumOfpqr :: Double -> Double -> Double -> Double
sumOfpqr a b c = p a b c + p b c a + p c a b
