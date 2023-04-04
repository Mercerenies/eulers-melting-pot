
-- Analysis of the behavior of N modulo K, where N is an integer as
-- defined in Problem 146 and K varies.

isPrime :: Integral i => i -> Bool
isPrime n = all (\k -> n `mod` k /= 0) [2..n-1]

isCoprime :: Integral i => i -> i -> Bool
isCoprime a b = gcd a b == 1

testValues :: Integral i => i -> [i]
testValues n = map (n ^ 2 +) [1, 3, 7, 9, 13, 27]

-- Valid values for N mod K, given K.
validValues :: Integral i => i -> [i]
validValues k = filter (all canPossiblyBePrime . testValues) [0..k-1]
    where canPossiblyBePrime j = isCoprime (j `mod` k) k

main :: IO ()
main = mapM_ go $ filter isPrime[2..500]
    where go k = putStrLn $ "n mod " ++ show k ++ " = " ++ show (validValues k)

{-
n mod 2 = [0]
n mod 3 = [1,2]
n mod 4 = [0,2]
n mod 5 = [0]
n mod 6 = [2,4]
n mod 7 = [3,4]
n mod 8 = [0,2,4,6]
n mod 9 = [1,2,4,5,7,8]
n mod 10 = [0]
n mod 11 = [0,1,4,5,6,7,10]
n mod 12 = [2,4,8,10]
n mod 13 = [1,3,4,9,10,12]
n mod 14 = [4,10]
n mod 15 = [5,10]
n mod 16 = [0,2,4,6,8,10,12,14]
n mod 17 = [0,1,3,6,7,8,9,10,11,14,16]
n mod 18 = [2,4,8,10,14,16]
n mod 19 = [0,1,2,3,6,8,9,10,11,13,16,17,18]
n mod 20 = [0,10]
n mod 21 = [4,10,11,17]
n mod 22 = [0,4,6,10,12,16,18]
n mod 23 = [0,1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22]
n mod 24 = [2,4,8,10,14,16,20,22]
n mod 25 = [0,5,10,15,20]
n mod 26 = [4,10,12,14,16,22]
n mod 27 = [1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26]
n mod 28 = [4,10,18,24]
n mod 29 = [0,1,2,3,5,6,8,9,10,11,13,16,18,19,20,21,23,24,26,27,28]
n mod 30 = [10,20]
n mod 31 = [0,1,3,4,5,6,8,9,10,12,13,14,15,16,17,18,19,21,22,23,25,26,27,28,30]
n mod 32 = [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]
n mod 33 = [1,4,5,7,10,11,16,17,22,23,26,28,29,32]
n mod 34 = [0,6,8,10,14,16,18,20,24,26,28]
n mod 35 = [10,25]
n mod 36 = [2,4,8,10,14,16,20,22,26,28,32,34]
n mod 37 = [0,1,2,3,4,5,7,8,9,10,12,13,14,15,22,23,24,25,27,28,29,30,32,33,34,35,36]
n mod 38 = [0,2,6,8,10,16,18,20,22,28,30,32,36]
n mod 39 = [1,4,10,14,16,17,22,23,25,29,35,38]
n mod 40 = [0,10,20,30]
n mod 41 = [0,1,2,3,4,5,6,7,8,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,28,29,30,31,33,34,35,36,37,38,39,40]
n mod 42 = [4,10,32,38]
n mod 43 = [0,1,2,3,5,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,38,40,41,42]
n mod 44 = [0,4,6,10,12,16,18,22,26,28,32,34,38,40]
n mod 45 = [5,10,20,25,35,40]
n mod 46 = [0,2,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,44]
n mod 47 = [0,1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,39,40,41,42,43,44,45,46]
n mod 48 = [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46]
n mod 49 = [3,4,10,11,17,18,24,25,31,32,38,39,45,46]
n mod 50 = [0,10,20,30,40]
n mod 51 = [1,7,8,10,11,14,16,17,20,23,25,26,28,31,34,35,37,40,41,43,44,50]
n mod 52 = [4,10,12,14,16,22,30,36,38,40,42,48]
n mod 53 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,25,28,31,32,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52]
n mod 54 = [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52]
n mod 55 = [0,5,10,15,40,45,50]
n mod 56 = [4,10,18,24,32,38,46,52]
n mod 57 = [1,2,8,10,11,13,16,17,19,20,22,25,28,29,32,35,37,38,40,41,44,46,47,49,55,56]
n mod 58 = [0,2,6,8,10,16,18,20,24,26,28,30,32,34,38,40,42,48,50,52,56]
n mod 59 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58]
n mod 60 = [10,20,40,50]
n mod 61 = [0,1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,21,22,23,24,25,26,30,31,35,36,37,38,39,40,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60]
n mod 62 = [0,4,6,8,10,12,14,16,18,22,26,28,30,32,34,36,40,44,46,48,50,52,54,56,58]
n mod 63 = [4,10,11,17,25,31,32,38,46,52,53,59]
n mod 64 = [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62]
n mod 65 = [10,25,30,35,40,55]
n mod 66 = [4,10,16,22,26,28,32,34,38,40,44,50,56,62]
n mod 67 = [0,1,2,3,4,5,6,7,9,10,12,13,14,15,16,17,18,19,20,21,22,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,45,46,47,48,49,50,51,52,53,54,55,57,58,60,61,62,63,64,65,66]
n mod 68 = [0,6,8,10,14,16,18,20,24,26,28,34,40,42,44,48,50,52,54,58,60,62]
n mod 69 = [1,2,5,7,8,10,11,13,14,16,17,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,52,53,55,56,58,59,61,62,64,67,68]
n mod 70 = [10,60]
n mod 71 = [0,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,50,51,52,53,54,55,56,57,58,59,60,61,62,64,65,66,67,68,69,70]
n mod 72 = [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70]
n mod 73 = [0,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,18,19,20,21,23,24,25,26,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,47,48,49,50,52,53,54,55,57,58,59,60,61,62,63,64,66,67,68,69,70,71,72]
n mod 74 = [0,2,4,8,10,12,14,22,24,28,30,32,34,36,38,40,42,44,46,50,52,60,62,64,66,70,72]
n mod 75 = [5,10,20,25,35,40,50,55,65,70]
n mod 76 = [0,2,6,8,10,16,18,20,22,28,30,32,36,38,40,44,46,48,54,56,58,60,66,68,70,74]
n mod 77 = [4,10,11,17,18,32,38,39,45,59,60,66,67,73]
n mod 78 = [4,10,14,16,22,38,40,56,62,64,68,74]
n mod 79 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78]
n mod 80 = [0,10,20,30,40,50,60,70]
n mod 81 = [1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50,52,53,55,56,58,59,61,62,64,65,67,68,70,71,73,74,76,77,79,80]
n mod 82 = [0,2,4,6,8,10,12,16,18,20,22,24,26,28,30,34,36,38,40,42,44,46,48,52,54,56,58,60,62,64,66,70,72,74,76,78,80]
n mod 83 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82]
n mod 84 = [4,10,32,38,46,52,74,80]
n mod 85 = [0,10,20,25,35,40,45,50,60,65,75]
n mod 86 = [0,2,8,10,12,14,16,18,20,22,24,26,28,32,34,36,38,40,42,44,46,48,50,52,54,58,60,62,64,66,68,70,72,74,76,78,84]
n mod 87 = [1,2,5,8,10,11,13,16,19,20,23,26,28,29,31,32,34,35,37,38,40,47,49,50,52,53,55,56,58,59,61,64,67,68,71,74,76,77,79,82,85,86]
n mod 88 = [0,4,6,10,12,16,18,22,26,28,32,34,38,40,44,48,50,54,56,60,62,66,70,72,76,78,82,84]
n mod 89 = [0,1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,77,78,79,80,81,82,83,84,85,86,87,88]
n mod 90 = [10,20,40,50,70,80]
n mod 91 = [3,4,10,17,25,38,53,66,74,81,87,88]
n mod 92 = [0,2,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,44,46,48,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,90]
n mod 93 = [1,4,5,8,10,13,14,16,17,19,22,23,25,26,28,31,32,34,35,37,40,41,43,44,46,47,49,50,52,53,56,58,59,61,62,65,67,68,70,71,74,76,77,79,80,83,85,88,89,92]
n mod 94 = [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,40,42,44,46,48,50,52,54,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92]
n mod 95 = [0,10,20,25,30,35,40,55,60,65,70,75,85]
n mod 96 = [2,4,8,10,14,16,20,22,26,28,32,34,38,40,44,46,50,52,56,58,62,64,68,70,74,76,80,82,86,88,92,94]
n mod 97 = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,23,24,25,27,28,29,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,67,68,69,70,72,73,74,76,77,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96]
n mod 98 = [4,10,18,24,32,38,46,52,60,66,74,80,88,94]
n mod 99 = [1,4,5,7,10,11,16,17,22,23,26,28,29,32,34,37,38,40,43,44,49,50,55,56,59,61,62,65,67,70,71,73,76,77,82,83,88,89,92,94,95,98]
n mod 100 = [0,10,20,30,40,50,60,70,80,90]
-}

------------

-- http://dynamicpublishers.com/Neural/NPSC2011/17-NPSC-31-05.pdf
--
-- Neural, Parallel, and Scientific Computations 19 (2011) 323-330
-- GENERATOR ALGORITHMS FOR PRIME k-TUPLES USING
-- BINOMIAL EXPRESSIONS
-- ZVI RETCHKIMAN KONIGSBERG

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- Return value starts with least-significant digit
toBaseP :: Integral i => i -> i -> [i]
toBaseP _ 0 = []
toBaseP p n = (n `mod` p) : toBaseP p (n `div` p)

countCarries :: Integral i => i -> i -> i -> Int
countCarries p a b = go 0 (toBaseP p a) (toBaseP p b)
    where go c [] _ = 0
          go c _ [] = 0
          go c (a:as) (b:bs) =
              let c' = if fromIntegral c + a + b >= p then 1 else 0 in
              c' + go c' as bs

c :: Integral i => i -> i -> Int
c n p = countCarries p (n + (p - 1) `div` 2 + (p - 1)) (p - 1)

iter :: Integral i => i -> [i] -> Bool
iter n ps = all (\p -> c n p == 0) ps
