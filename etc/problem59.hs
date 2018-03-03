
import Data.List.Split
import Data.Char
import Data.Bits
import Control.Monad
import Data.Maybe(maybeToList)

type Key = [Int]

text :: IO [Int]
text = map read . splitOn "," <$> readFile "./files/p059_cipher.txt"

decode :: Key -> [Int] -> [Int]
decode ks xs = zipWith xor (cycle ks) xs

possibleKeys :: [[Int]]
possibleKeys = replicateM 3 [0..255]

decodeAll :: [Int] -> [[Int]]
decodeAll xs = fmap (\ks -> decode ks xs) possibleKeys

matches :: [Int] -> Maybe String
matches xs = string <$ guard isValid
    where string = map chr xs
          isValid = all (\x -> isAlphaNum x || isSpace x || x `elem` "()!.,';") string

main :: IO ()
main = do
  t <- text
  print (length possibleKeys)
  let results = concatMap (maybeToList . matches) $ decodeAll t
  mapM_ print results
