module Parser(parseE, parseB, parseS, parse) where

import Lexer
import WhileAST


parseF :: [Token] -> Either String (AExpr, [Token])
parseF (TId v   : ts)          = Right (EVar v, ts)
parseF (TInt i  : ts)          = Right (EInt i, ts)
parseF (TMinus  : TInt i : ts) = Right (EInt $ -i, ts)
parseF (TLParen : ts)          =
  case parseE ts of
    Right (t, TRParen : ts) -> Right (t, ts)
    Right (_, t : _)        -> Left $ "Expected ')', got " ++ show t
    Right (_, [])           -> Left "Unexpected end of input. (Expected ')'.)"
    err                     -> err
parseF (t : _)                 = Left $ "Expected '-', '(', a variable identifier, or an integer literal, got " ++ show t
parseF _                       = Left "Unexpected end of input. (Expected '-', '(', a variable identifier, or an integer literal.)"

parseT :: [Token] -> Either String (AExpr, [Token])
parseT ts = do
  (e, ts) <- parseF ts
  parseT' e ts

parseT' :: AExpr -> [Token] -> Either String (AExpr, [Token])
parseT' e (TStar : ts) = do
  (e', ts) <- parseF ts
  parseT' (EBinOp AMul e e') ts
parseT' e ts           = Right (e, ts)

parseE :: [Token] -> Either String (AExpr, [Token])
parseE ts = do
  (e, ts) <- parseT ts
  parseE' e ts

parseE' :: AExpr -> [Token] -> Either String (AExpr, [Token])
parseE' e (t  : ts)
  | t == TPlus || t == TMinus = do
      (e', ts) <- parseT ts
      parseE' (EBinOp (tokToAOp t) e e') ts
  where
    tokToAOp TPlus  = AAdd
    tokToAOp TMinus = ASub
    tokToAOp _      = undefined
parseE' e ts                  = Right (e, ts)

parseA :: [Token] -> Either String (BExpr, [Token])
parseA (TTrue : ts)   = Right (BBool True, ts)
parseA (TFalse : ts)  = Right (BBool False, ts)
parseA (TLParen : ts) =
  case parseB ts of
    Right (e, TRParen : ts) -> Right (e, ts)
    Right (_, t : _)        -> Left $ "Expected ')', got " ++ show t
    Right (_, [])           -> Left "Unexpected end of input. (Expected ')'.)"
    err                     -> err
parseA ts =
  case parseE ts of
    Right (e, t : ts)
      | t == TEq || t == TLe -> do
          (e', ts) <- parseE ts
          return (BComp (tokToAComp t) e e', ts)
    Right (_, t : _)  -> Left $ "Expected '=' or '<=', got " ++ show t
    Right (_, [])     -> Left "Unexpected end of input. (Expected '=' or '<='.)"
    Left err          -> Left err
  where
    tokToAComp TEq = AEq
    tokToAComp TLe = ALe
    tokToAComp _   = undefined


parseC :: [Token] -> Either String (BExpr, [Token])
parseC (TBang : ts) = do
  (e, ts) <- parseA ts
  return (BNot e, ts)
parseC ts           = parseA ts

parseB :: [Token] -> Either String (BExpr, [Token])
parseB ts = do
  (e, ts) <- parseC ts
  parseB' e ts

parseB' :: BExpr -> [Token] -> Either String (BExpr, [Token])
parseB' e (TWedge : ts) = do
  (e', ts) <- parseC ts
  parseB' (BAnd e e') ts
parseB' e ts            = Right (e, ts)

parseI :: [Token] -> Either String (Stmt, [Token])
parseI (TId v : TAssign : ts) = do
  (e, ts) <- parseE ts
  return (SAssign v e, ts)
parseI (TIf : ts) =
  case parseB ts of
    Right (eC, TThen : ts) ->
      case parseI ts of
        Right (sT, TElse : ts) ->
          case parseI ts of
            Right (sE, ts) -> Right (SIte eC sT sE, ts)
            err            -> err
        Right (_, t : _) -> Left $ "Expected 'else', got " ++ show t
        Right _          -> Left "Unexpected end of input. (Expected 'else'.)"
        err              -> err
    Right (_, t : _) -> Left $ "Expected 'then', got " ++ show t
    Right _          -> Left "Unexpected end of input. (Expected 'then'.)"
    Left err         -> Left err
parseI (TWhile : ts)  = do
  (eC, ts) <- parseB ts
  (sB, ts) <- parseI ts
  return (SWhile eC sB, ts)
parseI (TLBrace : ts) =
  case parseS ts of
    Right (e, TRBrace : ts) -> Right (e, ts)
    Right (_, t : _)        -> Left $ "Expect '}', got " ++ show t
    Right _                 -> Left "Unexpected end of input. (Expected '}'.)"
    err                     -> err
parseI (t : _)        = Left $ "Expected 'if', 'while', '{', or a variable identifier, got " ++ show t
parseI _              = Left "Unexpected end of input. (Expected 'if', 'while', '{', or a variable identifier.)"

parseS :: [Token] -> Either String (Stmt, [Token])
parseS ts@(t : _)
  | isFirstI t = parseIS ts
  | otherwise  = Right (SSkip, ts)
  where
    isFirstI (TId _) = True
    isFirstI TIf     = True
    isFirstI TWhile  = True
    isFirstI TLBrace = True
    isFirstI _       = False

    parseIS ts = do
      (s, ts) <- parseI ts
      (s', ts) <- parseS ts
      return (SSeq s s', ts)
parseS [] = Right (SSkip, [])

parse :: String -> Either String Stmt
parse s =
  case parseS . scanTokens $ s of
    Right (s, []) -> Right s
    Right (_, ts) -> Left $ "Leftover tokens: " ++ show ts
    Left err      -> Left err

