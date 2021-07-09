
type Var = Char

data Instr = Repeat [Instr]
           | If [Instr] [Instr]
           | Incr
           | Decr
           | Number Int
           | ChOut
           | NumOut
           | Random
           | Set Var
           | Get Var
           | Add Var
           | Sub Var
           | Mul Var
           | Div Var
           | Mod Var
           | Input
           | Greater Var
           | Less Var
           | Eq Var
           | Date Int
           | Cls
           | Delay Int
             deriving (Eq)

type Code a = ([Instr], a)

instance Show Instr where

    showsPrec _ x =
        case x of
          Repeat xs -> ("(" ++) . shows xs . (")" ++)
          If xs ys -> ("{" ++) . shows xs . ("}{" ++) . shows ys . ("}" ++)
          Incr -> ("^" ++)
          Decr -> ("v" ++)
          Number n -> shows n
          ChOut -> ("a" ++)
          NumOut -> ("o" ++)
          Random -> ("r" ++)
          Set v -> ("~" ++) . (v :)
          Get v -> (v :)
          Add v -> ("+" ++) . (v :)
          Sub v -> ("-" ++) . (v :)
          Mul v -> ("*" ++) . (v :)
          Div v -> ("/" ++) . (v :)
          Mod v -> ("%" ++) . (v :)
          Input -> ("I" ++)
          Greater v -> (">" ++) . (v :)
          Less v -> ("<" ++) . (v :)
          Eq v -> ("=" ++) . (v :)
          Date n -> shows n . ("d" ++)
          Cls -> ("@" ++)
          Delay n -> shows n . (";" ++)

    showList = foldr (.) id . fmap shows

repeat' :: Code () -> Code ()
repeat' (x, ()) = ([Repeat x], ())

if' :: Code () -> Code () -> Code ()
if' (x, ()) (y, ()) = ([If x y], ())

incr :: Code ()
incr = ([Incr], ())

decr :: Code ()
decr = ([Decr], ())

number :: Int -> Code ()
number n = ([Number n], ())

chOut :: Code ()
chOut = ([ChOut], ())

numOut :: Code ()
numOut = ([NumOut], ())

random :: Code ()
random = ([Random], ())

set :: Var -> Code ()
set v = ([Set v], ())

get :: Var -> Code ()
get v = ([Get v], ())

add :: Var -> Code ()
add v = ([Add v], ())

sub :: Var -> Code ()
sub v = ([Sub v], ())

mul :: Var -> Code ()
mul v = ([Mul v], ())

div' :: Var -> Code ()
div' v = ([Div v], ())

mod' :: Var -> Code ()
mod' v = ([Mod v], ())

input :: Code ()
input = ([Input], ())

greater :: Var -> Code ()
greater v = ([Greater v], ())

less :: Var -> Code ()
less v = ([Less v], ())

eq :: Var -> Code ()
eq v = ([Eq v], ())

date :: Int -> Code ()
date n = ([Date n], ())

cls :: Code ()
cls = ([Cls], ())

delay :: Int -> Code ()
delay n = ([Delay n], ())

mov :: Var -> Var -> Code ()
mov dest src = get src >> set dest

-- Euclid's GCD algorithm; takes input via A and B, produces output
-- into A. Clobbers B, T.
gcd' :: Code ()
gcd' = do
  number 0
  repeat' $ do
    mov 'T' 'B'
    get 'A'
    mod' 'B'
    set 'B'
    mov 'A' 'T'
    get 'B'

-- LCM; takes input via A and B, produces output into A. Clobbers B,
-- T, S.
lcm' :: Code ()
lcm' = do
  get 'A'
  mul 'B'
  set 'S'
  gcd'
  get 'S'
  div' 'A'
  set 'A'

-- Count the number of times B goes into A. isPrime flag is in G.
-- Produces output into T and G, A is what's left. Clobbers B, S.
countFactors :: Code ()
countFactors = do
  get 'A'
  mul 'B'
  set 'A'
  number 0
  decr
  set 'T'
  number 0

  repeat' $ do

    number 0
    decr
    set 'S'
    get 'T'
    greater 'S'
    set 'S'
    get 'A'
    greater 'B'
    mul 'S'
    if' (number 1) $ do
      number 0
      set 'G'

    number 0
    set 'S'

    get 'T'
    incr
    set 'T'
    get 'A'
    div' 'B'
    set 'A'
    mod' 'B'
    eq 'S'

-- Find the smallest positive X such that 10^X mod A = 1. Pushes
-- result into X. Clobbers T, S.
a006556 :: Code ()
a006556 = do
  number 1
  set 'T'
  number 0
  set 'X'
  number 1
  repeat' $ do
    number 10
    set 'S'
    get 'T'
    mul 'S'
    mod' 'A'
    set 'T'
    get 'X'
    incr
    set 'X'
    get 'T'

-- Given A, assuming A is prime, calculating a(A). Stores result in X.
-- Clobbers T, S.
primeA :: Code ()
primeA = do
  number 3
  set 'X'
  number 3
  less 'A'
  if' (number 1) a006556

-- Given A, B, return A^B in A. Clobbers B, T.
power :: Code ()
power = do
  mov 'T' 'A'
  get 'B'
  incr
  set 'B'
  number 0
  repeat' $ do
    get 'A'
    mul 'T'
    set 'A'
    get 'B'
    decr
    set 'B'
  get 'A'
  div' 'T'
  div' 'T'
  set 'A'

{- n0->N  -  product->C  -  n->A  -  isprime->G  -  i->B  -  K,M are temp A,B  -  count->W  -  x->X -}

-- Given N, produce a(N) into C. Clobbers A, B, G, K, M, S, T, W, X.
fullA :: Code ()
fullA = do
  number 1
  set 'C'
  mov 'A' 'N'
  number 2
  set 'B'
  number 1
  set 'G'
  number 1
  repeat' $ do
    mov 'K' 'A'
    mov 'M' 'B'
    countFactors
    mov 'W' 'T'
    mov 'K' 'A'
    number 0
    if' (get 'W') $ do
      number 3
      set 'B'
    mov 'A' 'B'
    primeA
    number 0
    less 'W'
    if' (number 1) $ do
      get 'W'
      decr
      set 'B'
      mov 'A' 'M'
      power
      get 'A'
      mul 'X'
      set 'A'
      mov 'B' 'C'
      lcm'
      mov 'C' 'A'
    mov 'B' 'M'
    mov 'A' 'K'
    get 'B'
    incr
    set 'B'
    get 'B'
    get 'A'
  number 1
  if' (get 'G') $ do
    mov 'C' 'N'

-- Given sum in D, remaining in R and a number to test in N, test the
-- number. Clobbers... let's be real, the whole alphabet.
testValue :: Code ()
testValue = do
  fullA
  number 0
  set 'S'
  get 'N'
  decr
  mod' 'C'
  eq 'S'
  set 'T'
  get 'R'
  greater 'S'
  mul 'T'
  if' (number 1) $ do
    get 'R'
    decr
    set 'R'
    get 'D'
    add 'N'
    set 'D'

-- Just run it :)
fullProgram :: Code ()
fullProgram = do
  number 0
  set 'D'
  number 25
  set 'R'
  number 10
  set 'N'
  number 0
  repeat' $ do

    -- Test xxx1
    get 'N'
    incr
    set 'N'
    testValue

    -- Test xxx3
    get 'N'
    incr
    incr
    set 'N'
    testValue

    -- Test xxx7
    get 'N'
    incr
    incr
    incr
    incr
    set 'N'
    testValue

    -- Test xxx9
    get 'N'
    incr
    incr
    set 'N'
    testValue

    get 'N'
    incr
    set 'N'

    get 'R'

  get 'D'
  numOut

main :: IO ()
main =
    let (xs, ()) = fullProgram in
    print xs
