
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

p :: Double -> Double -> Double -> Double
p a b c = sqrt (4 * b ^ 2 * c ^ 2 * (1 - (wtilde a b c) ^ 2) / (3 * (b ^ 2 + c ^ 2 - 2 * b * c * wtilde a b c)))

sumOfpqr :: Double -> Double -> Double -> Double
sumOfpqr a b c = p a b c + p b c a + p c a b
