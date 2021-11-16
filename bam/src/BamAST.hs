module BamAST (Const(..), Instr(..),
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
  | IBranch Code Code
  | ILoop   Code Code
  | INoop
  deriving (Show)

type Code = [Instr]

-- Pretty-printing
indent :: Bool -> Int -> String -> String
indent False _ acc = acc
indent True  0 acc = acc
indent True  n acc = indent True (n - 1) $ "  " ++ acc

newline :: Bool -> String -> String
newline False acc = acc
newline True  acc = '\n' : acc

pp :: Bool -> Int -> Instr -> String
pp f d (IPush n    ) = indent f d $ "PUSH " ++ show n
pp f d  INot         = indent f d   "NOT"
pp f d  IAdd         = indent f d   "ADD"
pp f d  IMul         = indent f d   "MUL"
pp f d  ISub         = indent f d   "SUB"
pp f d  IAnd         = indent f d   "AND"
pp f d  IEq          = indent f d   "EQ"
pp f d  ILe          = indent f d   "LE"
pp f d (IFetch x   ) = indent f d $ "LOAD " ++ x
pp f d (IStore x   ) = indent f d $ "STORE " ++ x
pp f d (IBranch t e) = indent f d $ "IF("
                     ++ (newline f $ showCode f (d + 1) t)
                     ++ indent f (d + 1) ","
                     ++ (newline f $ showCode f (d + 1) e)
                     ++ ")"
pp f d (ILoop c b  ) = indent f d $ "LOOP("
                     ++ (newline f $ showCode f (d + 1) c)
                     ++ indent f (d + 1) ","
                     ++ (newline f $ showCode f (d + 1) b)
                     ++ ")"
pp f d  INoop        = indent f d   "NOOP"

showCode :: Bool -> Int -> Code -> String
showCode _ _ []    = []
showCode f d [i]   = pp f d i ++ ";" ++ if f then "\n" else ""
showCode f d (i : c) = pp f d i ++ ";" ++ (newline f $ showCode f d c)

showFlatCode :: Code -> String
showFlatCode = showCode False 0

