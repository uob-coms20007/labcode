module Parser(parseE, parseB, parseS, parse) where

import Lexer
import WhileAST


parseF :: [Token] -> Either String (AExpr, [Token])
parseF = undefined

parseT :: [Token] -> Either String (AExpr, [Token])
parseT = undefined

parseT' :: AExpr -> [Token] -> Either String (AExpr, [Token])
parseT' = undefined

parseE :: [Token] -> Either String (AExpr, [Token])
parseE = undefined

parseE' :: AExpr -> [Token] -> Either String (AExpr, [Token])
parseE' = undefined

parseA :: [Token] -> Either String (BExpr, [Token])
parseA = undefined

parseC :: [Token] -> Either String (BExpr, [Token])
parseC = undefined

parseB :: [Token] -> Either String (BExpr, [Token])
parseB = undefined

parseB' :: BExpr -> [Token] -> Either String (BExpr, [Token])
parseB' = undefined

parseI :: [Token] -> Either String (Stmt, [Token])
parseI = undefined

parseS :: [Token] -> Either String (Stmt, [Token])
parseS = undefined

parse :: String -> Either String Stmt
parse s =
  case parseS . scanTokens $ s of
    Right (t, []) -> Right t
    Right (_, ts) -> Left $ "Leftover tokens: " ++ show ts
    Left err      -> Left err

