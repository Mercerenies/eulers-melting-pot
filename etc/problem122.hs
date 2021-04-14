
-- Impossible to brute force. VERY impossible :P

import Control.Monad(guard)

allProducts :: Int -> [Int] -> Int
allProducts target xs
    | target `elem` xs = length xs - 1 -- Minus one because we don't count the "initial" information of n
    | otherwise = minimum $ do
                    a <- xs
                    b <- xs
                    guard (a <= b)
                    guard (a + b <= target)
                    guard ((a + b) `notElem` xs)
                    return $ allProducts target ((a + b):xs)

main :: IO ()
main = print (fmap (\n -> allProducts n [1]) [1..10])
