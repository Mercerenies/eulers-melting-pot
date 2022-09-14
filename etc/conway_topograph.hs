{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

import Control.Arrow

data Sign = Neg | Pos
            deriving (Show, Read, Eq, Ord, Enum)

neg :: Sign -> Sign
neg Neg = Pos
neg Pos = Neg

act :: Num a => Sign -> a -> a -> a
act Neg = (-)
act Pos = (+)

act2 :: Num a => Sign -> Vector a -> Vector a -> Vector a
act2 s (x, y) (x', y') = (act s x x', act s y y')

--  \ front /
--   \     /
--    L   R
--     \ /
--      *
-- left | right
--      B
--      |
data Conway a = Conway {
      frontValue :: a,
      leftValue :: a,
      rightValue :: a,
      leftBranch :: (Vector a, Branch a),
      rightBranch :: (Vector a, Branch a),
      backBranch :: (Vector a, Branch a)
    }

-- \  N  /
--  L   R
--   \ /
--    *
data Branch a = Branch {
      leftPath :: (Vector a, Branch a),
      rightPath :: (Vector a, Branch a),
      obverseVector :: Vector a,
      branchValue :: a
    }

type Vector a = (a, a)

flipBranch :: Branch a -> Branch a
flipBranch (Branch { .. }) =
    Branch {
      leftPath = second flipBranch rightPath,
      rightPath = second flipBranch leftPath,
      obverseVector = obverseVector,
      branchValue = branchValue
    }

flipConway :: Conway a -> Conway a
flipConway (Conway { .. }) =
    Conway {
      frontValue = frontValue,
      leftValue = rightValue,
      rightValue = leftValue,
      leftBranch = second flipBranch rightBranch,
      rightBranch = second flipBranch leftBranch,
      backBranch = second flipBranch backBranch
    }

rotateClockwise :: Conway a -> Conway a
rotateClockwise (Conway { .. }) =
    Conway {
      frontValue = rightValue,
      leftValue = frontValue,
      rightValue = leftValue,
      leftBranch = rightBranch,
      rightBranch = backBranch,
      backBranch = leftBranch
    }

rotateCounter :: Conway a -> Conway a
rotateCounter = rotateClockwise . rotateClockwise

--      |
--      |
--      |
-- -----X
--       \     /
--        L   R
--         \ /
--          *
--          |
--          B
--          |
moveLeft :: Conway a -> Conway a
moveLeft (Conway { .. }) =
    Conway {
      frontValue = branchValue (snd leftBranch),
      leftValue = leftValue,
      rightValue = frontValue,
      leftBranch = leftPath (snd leftBranch),
      rightBranch = rightPath (snd leftBranch),
      backBranch = (fst leftBranch, Branch {
                              leftPath = rightBranch,
                              rightPath = backBranch,
                              obverseVector = fst leftBranch,
                              branchValue = rightValue
                            })
    }

moveRight :: Conway a -> Conway a
moveRight = flipConway >>> moveLeft >>> flipConway

moveBackLeft :: Conway a -> Conway a
moveBackLeft = rotateCounter >>> moveLeft >>> rotateCounter

moveBackRight :: Conway a -> Conway a
moveBackRight = rotateClockwise >>> moveRight >>> rotateClockwise

conway :: forall a. Num a => a -> a -> a -> Conway a
conway leftValue rightValue frontValue =
    Conway {
      frontValue = frontValue,
      leftValue = leftValue,
      rightValue = rightValue,
      leftBranch = ((0, 1), goBranch leftValue frontValue rightValue (Neg, (1, 0)) (Pos, (1, 1)) (Neg, (0, 1))),
      rightBranch = ((1, 0), goBranch frontValue rightValue leftValue (Pos, (1, 1)) (Neg, (0, 1)) (Neg, (1, 0))),
      backBranch = ((1, 1), goBranch rightValue leftValue frontValue (Neg, (0, 1)) (Neg, (1, 0)) (Pos, (1, 1)))
    }
  where goBranch :: a -> a -> a -> (Sign, Vector a) -> (Sign, Vector a) -> (Sign, Vector a) -> Branch a
        goBranch l r b (ls, lv) (rs, rv) (bs, bv) =
            let f = 2 * (l + r) - b
                fs = neg bs
                fv = act2 fs lv rv in
            Branch {
              leftPath = (rv, goBranch l f r (ls, lv) (fs, fv) (rs, rv)),
              rightPath = (lv, goBranch f r l (fs, fv) (rs, rv) (rs, rv)),
              obverseVector = bv,
              branchValue = f
            }

simplePrint :: Show a => Conway a -> IO ()
simplePrint (Conway {..}) = do
  putStrLn $ "Front: f" ++ show (fst backBranch) ++ " = " ++ show frontValue
  putStrLn $ "Left: f" ++ show (fst rightBranch) ++ " = " ++ show leftValue
  putStrLn $ "Right: f" ++ show (fst leftBranch) ++ " = " ++ show rightValue

main :: IO ()
main = pure ()
