{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Underload code generator.

newtype Code = Code [Instruction]
    deriving (Eq, Semigroup, Monoid)

data Instruction = Swap
                 | Dup
                 | Discard
                 | Concat
                 | Enclose
                 | Eval
                 | Print
                 | Literal [Instruction]
                 | LiteralText String
                 deriving (Eq)

instance Show Instruction where
    showsPrec _ instr =
        case instr of
          Swap -> ("~" ++)
          Dup -> (":" ++)
          Discard -> ("!" ++)
          Concat -> ("*" ++)
          Enclose -> ("a" ++)
          Eval -> ("^" ++)
          Print -> ("S" ++)
          Literal instrs -> ("(" ++) . foldShows (map shows instrs) . (")" ++)
          LiteralText s -> ("(" ++) . (s ++) . (")" ++)

instance Show Code where
    showsPrec _ (Code code) = foldShows (map shows code)

foldShows :: [ShowS] -> ShowS
foldShows = foldr (.) id

singleton :: Instruction -> Code
singleton x = Code [x]

lit :: Code -> Code
lit (Code instrs) = singleton $ Literal instrs

emptyLit :: Code
emptyLit = lit mempty

litText :: String -> Code
litText = singleton . LiteralText

nop :: Code
nop = emptyLit <> discardI

swapI, dupI, discardI, concatI, encloseI, evalI, printI :: Code
swapI = singleton Swap
dupI = singleton Dup
discardI = singleton Discard
concatI = singleton Concat
encloseI = singleton Enclose
evalI = singleton Eval
printI = singleton Print

swapCatI :: Code
swapCatI = swapI <> concatI

mpow :: Monoid m => m -> Int -> m
mpow x n = mconcat $ replicate n x

-- Only supports a handful of small nonnegative integers right now.
--
-- Source: https://esolangs.org/wiki/Underload/Numbers
pushNumber :: Int -> Code
pushNumber n = lit (go n)
    where go 0 = discardI <> emptyLit
          go 1 = nop
          go 2 = dupI <> concatI
          go 3 = dupI <> dupI <> concatI <> concatI
          go 4 = (dupI <> concatI) `mpow` 2
          go 5 = dupI <> dupI <> concatI <> dupI <> concatI <> concatI
          go 6 = dupI <> dupI <> concatI <> concatI <> dupI <> concatI
          go 7 = dupI `mpow` 3 <> concatI `mpow` 2 <> dupI <> concatI `mpow` 2
          go 8 = (dupI <> concatI) `mpow` 3
          go 9 = dupI <> dupI <> concatI <> dupI <> concatI <> dupI <> concatI <> concatI
          go n = error $ "pushNumber: not a small nonnegative integer: " ++ show n

-- Stack effect: ( ..a n -- ..b ) assuming each branch has effect ( ..a -- ..b )
--
-- Branch instruction: chooses which branch to take by popping the top
-- stack element. If the list of branches has length N, then the top
-- stack element must be an integer from 0 (representing the first
-- branch in the list) up to N-1 (the last branch in the list), in
-- Church encoding representation.
branch :: [Code] -> Code
branch codes = lookupTable <> swapI <>
               lit (evalI <> lit discardI) <> swapCatI <>
               lit (evalI <> evalI) <> concatI <>
               evalI
    where lookupTable :: Code
          lookupTable = lit $ foldMap tableEntry . zip [0..] $ reverse codes
          tableEntry :: (Int, Code) -> Code
          tableEntry (n, c) = lit $ discardI `mpow` n <> c

branchExample :: Code
branchExample = pushNumber 4 <>
                branch [
                 -- Case 0
                 litText "Case 0" <> printI,
                 -- Case 1
                 litText "Case 1" <> printI,
                 -- Case 2
                 litText "Case 2" <> printI,
                 -- Case 3
                 litText "Case 3" <> printI,
                 -- Case 4
                 litText "Case 4" <> printI,
                 -- Case 5
                 litText "Case 5" <> printI,
                 -- Case 6
                 litText "Case 6" <> printI
                ]


-- Stack effect: ( x -- x )
numToUnary :: Char -> Code
numToUnary ch = litText [ch] <> swapI <> evalI

-- Stack effect: ( x y -- z )
mul :: Code
mul = concatI

-- Stack effect: ( x y -- z )
pow :: Code
pow = evalI

-- Stack effect: ( x y -- z )
add :: Code
add = lit swapI <> concatI <>
      lit dupI <> swapCatI <>
      swapI <> lit concatI <> concatI <> concatI

main :: IO ()
main = do
  let code = pushNumber 3 <> pushNumber 8 <> add <> numToUnary 'x' <> printI
  print code
