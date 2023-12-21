{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module Main where

import Data.Bits
import Control.Monad
import System.Directory
import System.FilePath

-- Haskell program to build the Folders structures.

data Command = If Expr [Command]
             | While Expr [Command]
             | Declare Type VarName
             | Let VarName Expr
             | Print Expr
             | Input VarName
               deriving (Show, Eq)

newtype VarName = VarName Int
    deriving (Show, Read, Eq, Ord)

data Expr = Var VarName
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | LiteralInt Int
          -- NOTE: I'm not translating the other literals unless I need them :)
          | EqualTo Expr Expr
          | GreaterThan Expr Expr
          | LessThan Expr Expr
            deriving (Show, Eq)

data Type = TInt | TFloat | TString | TChar
            deriving (Show, Eq)

data Folder = Folder { folderName :: String, folderContents :: [Folder] }
    deriving (Show, Eq)

simpleFolder :: String -> Int -> Folder
simpleFolder name n = Folder name $ replicate n (emptyFolder "")

emptyFolder :: String -> Folder
emptyFolder name = Folder name []

class Compilable a where
    compile :: a -> Folder

instance Compilable Command where
    compile (If expr commands) = Folder "if" [simpleFolder "" 0, compile expr, compile commands]
    compile (While expr commands) = Folder "while" [simpleFolder "" 1, compile expr, compile commands]
    compile (Declare type_ varName) = Folder "declare" [simpleFolder "" 2, compile type_, compile varName]
    compile (Let varName expr) = Folder "let" [simpleFolder "" 3, compile varName, compile expr]
    compile (Print expr) = Folder "print" [simpleFolder "" 4, compile expr]
    compile (Input varName) = Folder "input" [simpleFolder "" 5, compile varName]

instance Compilable VarName where
    compile (VarName n) = simpleFolder ("var " ++ show n) n

instance Compilable Expr where
    compile (Var varName) = Folder "variable" [simpleFolder "" 0, compile varName]
    compile (Add lhs rhs) = Folder "add" [simpleFolder "" 1, compile lhs, compile rhs]
    compile (Sub lhs rhs) = Folder "subtract" [simpleFolder "" 2, compile lhs, compile rhs]
    compile (Mul lhs rhs) = Folder "multiply" [simpleFolder "" 3, compile lhs, compile rhs]
    compile (Div lhs rhs) = Folder "divide" [simpleFolder "" 4, compile lhs, compile rhs]
    compile (LiteralInt n) = Folder "literal" [simpleFolder "" 5, compile TInt, compile n]
    compile (EqualTo lhs rhs) = Folder "equalto" [simpleFolder "" 6, compile lhs, compile rhs]
    compile (GreaterThan lhs rhs) = Folder "greaterthan" [simpleFolder "" 7, compile lhs, compile rhs]
    compile (LessThan lhs rhs) = Folder "lessthan" [simpleFolder "" 8, compile lhs, compile rhs]

instance Compilable Type where
    compile TInt = simpleFolder "int" 0
    compile TFloat = simpleFolder "float" 1
    compile TString = simpleFolder "string" 2
    compile TChar = simpleFolder "char" 3

instance Compilable [Command] where
    compile = Folder "commands" . map compile

instance Compilable Int where
    compile n = Folder "" [Folder "" (map compile (take 4 bits)), Folder "" (map compile (drop 4 bits))]
      where bits = lowestByte n

instance Compilable Bool where
    compile b = simpleFolder "" (if b then 1 else 0)

lowestByte :: Int -> [Bool]
lowestByte n = [n .&. shift 1 i /= 0 | i <- [7,6..0]]

padLeft :: Int -> a -> [a] -> [a]
padLeft n a as = (take (n - length as) (repeat a)) ++ as

-- Add prefixes to all folder names so that the folders are in
-- alphabetical order.
prefixFolderNames :: [Folder] -> [Folder]
prefixFolderNames folders = zipWith go [1..length folders] folders
    where folderPrefix n = "New Folder (" ++ show n ++ ")"
          applyPrefix prefix "" = prefix
          applyPrefix prefix suffix = prefix ++ " - " ++ suffix
          go n (Folder {..}) = Folder (applyPrefix (folderPrefix n) folderName) (prefixFolderNames folderContents)

buildProgramFolders :: FilePath -> Folder -> IO ()
buildProgramFolders rootPath (Folder {..}) = do
  let newFolderName = rootPath </> folderName
  createDirectory newFolderName
  if null folderContents then
      writeFile (newFolderName </> ".gitkeep") ""
  else
      forM_ folderContents (buildProgramFolders newFolderName)

-- 250,002
upperLimit :: Expr
upperLimit = (Add (Mul (LiteralInt 25) (Mul (LiteralInt 100) (LiteralInt 100))) (LiteralInt 2))

-- 1,000,001
tooManyTiles :: Expr
tooManyTiles = Sub (Mul upperLimit (LiteralInt 4)) (LiteralInt 7)

tilesNeeded :: VarName -> VarName -> Expr
tilesNeeded n m = Sub (Mul (Var n) (Var n)) (Mul (Var m) (Var m))

-- "And" is just multiplication in Folders.
foldersAnd :: Expr -> Expr -> Expr
foldersAnd = Mul

varTotal :: VarName
varTotal = VarName 0

varN :: VarName
varN = VarName 1

varM :: VarName
varM = VarName 2

eulerProgram :: [Command]
eulerProgram = [
    Declare TInt varTotal,
    Declare TInt varN,
    Declare TInt varM,
    Let varTotal (LiteralInt 0),
    Let varN (LiteralInt 3),
    While (LessThan (Var varN) upperLimit) [
      Let varM (Sub (Var varN) (LiteralInt 2)),
      While (GreaterThan (Var varM) (LiteralInt 0) `foldersAnd` LessThan (tilesNeeded varN varM) tooManyTiles) [
        Let varM (Sub (Var varM) (LiteralInt 2)),
        Let varTotal (Add (Var varTotal) (LiteralInt 1))
      ],
      Let varN (Add (Var varN) (LiteralInt 1))
    ],
    Print (Var varTotal)
  ]

rootFolderName :: FilePath
rootFolderName = "problem173.folders"

main :: IO ()
main = do
  let compiledProgram = map compile eulerProgram
      compiledProgram' = prefixFolderNames compiledProgram
  cwd <- getCurrentDirectory
  rootFolderExists <- doesPathExist (cwd </> rootFolderName)
  when rootFolderExists $ removeDirectoryRecursive (cwd </> rootFolderName)
  buildProgramFolders cwd (Folder rootFolderName compiledProgram')
  putStrLn "Done."
