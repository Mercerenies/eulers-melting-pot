
π :: Double
π = pi

a, b, c :: Double
a = 399
b = 455
c = 511

w :: Double -> Double -> Double -> Double
w a b c = (b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c)

wtilde :: Double -> Double -> Double -> Double
wtilde a b c = w a b c / 2 - (sqrt 3 / 2) * sqrt (1 - w a b c ^ 2)

a', b', c' :: Double -> Double
a' a = a / sqrt 3
b' b = b / sqrt 3
c' c = c / sqrt 3

z :: Double -> Double -> Double -> Double
z a b c = sqrt (b' b ^ 2 + c' c ^ 2 - 2 * b' b * c' c * wtilde a b c)

y :: Double -> Double -> Double -> Double
y a b c = (c' c) * sqrt (1 - (wtilde a b c) ^ 2) / z a b c

p :: Double -> Double -> Double -> Double
p a b c = 2 * b' b * y a b c

sumOfpqr :: Double -> Double -> Double -> Double
sumOfpqr a b c = p a b c + p b c a + p c a b
