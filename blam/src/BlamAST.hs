module BlamAST (Const(..), Instr(..),
              Code, showCode, showFlatCode) where

data Const =
    CNum  Integer
  | CBool Bool

instance Show Const
  where
    show (CNum n ) = show n
    show (CBool b) = show b

data Instr =
    IPush Const
  | IAdd | IMul   | ISub
  | INot | IAnd   | IEq   | ILe
  | IFetch String | IStore String
  | IGoto  Int
  | IGotoF Int
  | INoop
  deriving (Show)

type Code = [Instr]

-- Pretty-printing
newline :: Bool -> String -> String
newline False acc = acc
newline True  acc = '\n' : acc

pp :: Instr -> String
pp (IPush n    ) = "PUSH " ++ show n
pp  INot         = "NOT"
pp  IAdd         = "ADD"
pp  IMul         = "MUL"
pp  ISub         = "SUB"
pp  IAnd         = "AND"
pp  IEq          = "EQ"
pp  ILe          = "LE"
pp (IFetch x   ) = "LOAD " ++ x
pp (IStore x   ) = "STORE " ++ x
pp (IGoto  l   ) = "GOTO " ++ show l
pp (IGotoF l   ) = "GOTOF " ++ show l
pp  INoop        = "NOOP"

showCode :: Bool -> Code -> String
showCode _ []    = []
showCode f [i]   = pp i ++ ";" ++ if f then "\n" else ""
showCode f (i : c) = pp i ++ ";" ++ newline f  (showCode f c)

showFlatCode :: Code -> String
showFlatCode = showCode False

