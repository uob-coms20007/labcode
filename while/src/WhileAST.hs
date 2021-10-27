module WhileAST where

type Var = String

-- Some helper functions to pretty-print
paren :: Bool -> String -> String
paren True  s = "(" ++ s ++ ")"
paren False s = s

-- The types defining our Abstract Syntax Trees for the While language

-- | /AExpr/ defines arithmetic expressions:
data AExpr =
  -- Variables are arithmetic expressions
    EVar Var
  -- Integer literals (in ZZ) are arithmetic expressions
  | EInt Integer
  -- Two expressions can be combined into a third using +, - or *
  | EAdd AExpr AExpr
  | ESub AExpr AExpr
  | EMul AExpr AExpr
  deriving (Show, Eq)

-- A pretty printer for arithmetic expressions
ppAexpr :: AExpr -> String
ppAexpr = aux False
  where
    aux _ (EInt n)     = show n
    aux _ (EVar v)     = v
    aux f (EAdd a1 a2) = paren f $ aux False a1 ++ " + " ++ aux False a2
    aux f (ESub a1 a2) = paren f $ aux False a1 ++ " - " ++ aux True a2
    aux _ (EMul a1 a2) = aux True a1 ++ " * " ++ aux True a2

-- | /BExpr/ defines boolean expressions:
data BExpr =
  -- Boolean literals are boolean expressions
    BBool Bool
  -- The negation of a boolean expression is a boolean expression
  | BNot BExpr
  -- The conjunction of two boolean expressions is a boolean expression
  | BAnd BExpr BExpr
  -- The comparison of two arithmetic expressions is a boolean expression (using = or ≤)
  | BEq AExpr AExpr
  | BLe AExpr AExpr
  deriving (Show, Eq)

-- A pretty printer for boolean expressions
ppBExpr :: BExpr -> String
ppBExpr = aux False
  where
    aux _ (BBool b)    = show b
    aux _ (BEq a1 a2)  = show a1 ++ " = " ++ show a2
    aux _ (BLe a1 a2)  = show a1 ++ " <= " ++ show a2
    aux _ (BNot b)     = "!" ++ aux True b
    aux f (BAnd b1 b2) = paren f $ aux False b1 ++ " && " ++ aux False b2

-- | /Stmt/ defines statements:
data Stmt =
  -- Empty statements are statements
    SSkip
  -- Assignments are statements
  | SAssign Var AExpr
  -- If-then-else is a statements (SIte c t e represents if c then t else e)
  | SIte    BExpr Stmt Stmt
  -- While loops are statements (SWhile c b represents while c b)
  | SWhile  BExpr Stmt
  -- The sequential composition of two statements is a statement
  | SSeq    Stmt Stmt
  deriving (Show, Eq)

-- A pretty-printer for statements
-- It uses braces as it GOTO fail was a thing
indent :: Int -> String -> String
indent 0 s = s
indent n s
  | 0 < n     = '\t' : indent (n - 1) s
  | otherwise = undefined

ppStmt :: Stmt -> String
ppStmt = aux 0
  where
    aux n (SAssign x e)  = indent n $ x ++ " := " ++ show e ++ "\n"
    aux n (SSeq s1 s2)   = aux n s1 ++ aux n s2
    aux n (SIte c s1 s2) =    indent n $ "if " ++ show c ++ "\n"
                           ++ indent n "then {\n"
                           ++ aux (n + 1) s1
                           ++ indent n "} else {\n"
                           ++ aux (n + 1) s2
                           ++ indent n "}\n"
    aux n (SWhile c s)   =    indent n $ "while " ++ show c ++ " {\n"
                           ++ aux (n + 1) s
                           ++ indent n "}\n"
    aux _ SSkip          = ""

