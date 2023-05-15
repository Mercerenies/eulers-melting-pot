
-- The problem can essentially be boiled down to this. Start with the
-- base capacitance (WLOG assume it's 1). We have eighteen instances
-- of that number, and we can add (+) or parallel add (||) those
-- instances, parenthesizing where we choose.
--
-- Both operations are commutative and associative. So to remove
-- duplicates, we can assume that the arguments to a sequence of + or
-- '||' operators must themselves be monotonically increasing in size.
-- Furthermore, we assume that an argument to + must be an application
-- of '||' and vice versa (treating both operators as variadic).
--
-- Written this way, the problem is phrased as follows: Generate all
-- trees with at most 18 leaf nodes, such that every node X satisfies
-- the following coherence properties:
--
-- * Either X is a leaf node (has no children) or X has at least two
--   children.
--
-- * If a child Y of X precedes a child Z of X, then Y has at most as
--   many children as Z.
--
-- Then, for each tree, try with + at the top and with || at the top.
-- Once we decide the top operation, each subsequent one must be
-- alternating.
--
-- Let's see if this is enough to reduce the problem complexity down
-- to being computable in reasonable time.

import Control.Monad
import Data.List
import qualified Data.Set as Set

data Op = Add | Par
          deriving (Show, Read, Eq, Ord, Enum)

data Tree a = Leaf a | Branch Int [Tree a]
              deriving (Show, Read, Eq)

branch :: [Tree a] -> Tree a
branch xs = Branch (sum $ map leafCount xs) xs

childCount :: Tree a -> Int
childCount (Leaf _) = 0
childCount (Branch _ xs) = length xs

leafCount :: Tree a -> Int
leafCount (Leaf _) = 1
leafCount (Branch n _) = n

other :: Op -> Op
other Add = Par
other Par = Add

(|+|) :: Fractional a => a -> a -> a
x |+| y = recip (recip x + recip y)

apply :: Fractional a => Op -> a -> a -> a
apply Add = (+)
apply Par = (|+|)

generate :: Int -> Int -> [Tree ()]
generate _ 0 = []
generate minChildren maxLeaves = [Leaf ()] ++ (msum $ map (\n -> branch <$> generateChildren n 2 maxLeaves) [minChildren..maxLeaves])

generateChildren :: Int -> Int -> Int -> [[Tree ()]]
generateChildren _ _ maxLeaves | maxLeaves < 0 = []
generateChildren 0 _ _ = [[]]
generateChildren count minChildren maxLeaves = do
  firstChild <- generate minChildren (maxLeaves - (count - 1))
  let leavesSpent = leafCount firstChild
      firstChildCount = childCount firstChild
      newMinChildren = max minChildren firstChildCount
      newMaxLeaves = maxLeaves - leavesSpent
  restChildren <- generateChildren (count - 1) newMinChildren newMaxLeaves
  return $ firstChild : restChildren

generateTrees :: Int -> [Tree ()]
generateTrees maxLeaves = generate 2 maxLeaves

evalTree :: Op -> Tree () -> Rational
evalTree _ (Leaf ()) = 1
evalTree op (Branch _ xs) =
    let xs' = map (evalTree (other op)) xs in
    foldl1' (apply op) xs'

evalTreeForOps :: Tree () -> [Rational]
evalTreeForOps tree = map (\op -> evalTree op tree) [Add, Par]

run :: Int -> Int
run maxLeaves =
    let trees = generateTrees maxLeaves
        capacitances = concatMap evalTreeForOps trees in
    Set.size (Set.fromList capacitances)

main :: IO ()
main = do
  print $ run 11
