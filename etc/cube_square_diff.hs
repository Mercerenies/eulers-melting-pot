
import Data.List

main :: IO ()
main = print . nub . sort . filter (> 0) $  [d ^ 3 - w ^ 2 | d <- [1..30], w <- [1..30]]
