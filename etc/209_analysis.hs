
import Data.Char

newtype Var = Var Int
    deriving (Eq)

instance Show Var where
    showsPrec _ (Var i) = showChar (chr (ord 'a' + i))

aa, bb, cc, dd, ee, ff :: Var
aa = Var 0
bb = Var 1
cc = Var 2
dd = Var 3
ee = Var 4
ff = Var 5

data Expr = EVar Var | EAnd Expr Expr | EXor Expr Expr
            deriving (Eq)

instance Show Expr where
    showsPrec n (EVar v) = showsPrec n v
    showsPrec n (EAnd e1 e2) = showParen (n > 3) $ showsPrec 3 e1 . showString " × " . showsPrec 3 e2
    showsPrec n (EXor e1 e2) = showParen (n > 2) $ showsPrec 2 e1 . showString " + " . showsPrec 2 e2

-- A Boolean function in n arguments can be realized
newtype BooleanPerm = BooleanPerm [Var]
