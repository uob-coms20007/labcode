module Goedel where

import WhileAST
import Parser (parse)
import Data.Either (fromRight)

encodeBool :: Bool -> Integer
encodeBool = undefined

decodeBool :: Integer -> Bool
decodeBool = undefined

delta :: Integer -> Integer
delta = undefined

pairHU :: Integer -> Integer -> Integer
pairHU = undefined

unpairHU :: Integer -> (Integer, Integer)
unpairHU = undefined

pair :: Integer -> Integer -> Integer
pair = undefined

unpair :: Integer -> (Integer, Integer)
unpair = undefined


-- Encoding lists of numbers

encodeList :: [Integer] -> Integer
encodeList [] = 0
encodeList (n:ns) = pair n (encodeList ns) + 1

decodeList :: Integer -> [Integer]
decodeList 0 = []
decodeList n = let (x, m) = unpair (n - 1) in (x : decodeList m)


-- Encoding binary trees


data BTree = Empty | Fork Integer BTree BTree
  deriving (Eq, Show)

insert :: Integer -> BTree -> BTree
insert x Empty = Fork x Empty Empty
insert x (Fork n t1 t2) =
  if x <= n then
    Fork n (insert x t1) t2
  else
    Fork n t1 (insert x t2)

tExample :: BTree
tExample = foldr insert Empty [10, 8, 2, 5]

encodeBTree :: BTree -> Integer
encodeBTree = undefined

decodeBTree :: Integer -> BTree
decodeBTree = undefined


-- Encoding variables

encodeVar :: String -> Integer
encodeVar ('x':s) = read s
encodeVar _       = error "not a valid variable"

decodeVar :: Integer -> String
decodeVar n = "x" ++ show n

-- Encoding WHILE programs

encodeAExpr :: AExpr -> Integer
encodeAExpr (EVar xs)           =     5 * encodeVar xs
encodeAExpr (EInt n)            = 1 + 5 * n
encodeAExpr (EBinOp AAdd a1 a2) = 2 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)
encodeAExpr (EBinOp ASub a1 a2) = 3 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)
encodeAExpr (EBinOp AMul a1 a2) = 4 + 5 * pair (encodeAExpr a1) (encodeAExpr a2)

decodeAExpr :: Integer -> AExpr
decodeAExpr = undefined

encodeBExpr :: BExpr -> Integer
encodeBExpr = undefined

decodeBExpr :: Integer -> BExpr
decodeBExpr = undefined

encodeStmt :: Stmt -> Integer
encodeStmt = undefined

decodeStmt :: Integer -> Stmt
decodeStmt = undefined

astExample1 :: Stmt
astExample1 =
  SSeq
    (SAssign "x1" (EInt 1))
    (SAssign "x2" (EBinOp AAdd (EInt 4) (EInt 2)))

astExample2 :: Stmt
astExample2 =
  fromRight SSkip $ parse $ unlines [
    "x1 := x1 + x2",
    "x1 := x2 - x1"
  ]

astExample3 :: Stmt
astExample3 =
  fromRight SSkip $ parse $ unlines [
    "x1 := x1 + x2",
    "x1 := x2 - x1",
    "x2 := x2 - x1"
  ]

