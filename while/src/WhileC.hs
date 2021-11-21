module WhileC (compile, main) where

import System.Environment
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))

import WhileAST
import BamAST
import qualified Parser(parse)

aritToInstrs :: AExpr -> Code -> Code
aritToInstrs (EInt n)     acc = IPush  (CNum n) : acc
aritToInstrs (EVar x)     acc = IFetch x : acc
aritToInstrs (EBinOp o a1 a2) acc = aritToInstrs a1 $ aritToInstrs a2 $ opToInstr o : acc
  where
    opToInstr AAdd = IAdd
    opToInstr ASub = ISub
    opToInstr AMul = IMul

boolToInstrs :: BExpr -> Code -> Code
boolToInstrs (BBool b)       acc = IPush (CBool b)  : acc
boolToInstrs (BComp o a1 a2) acc = aritToInstrs a1 $ aritToInstrs a2 $ opToInstr o  : acc
  where
    opToInstr AEq = IEq
    opToInstr ALe = ILe
boolToInstrs (BNot b)        acc = boolToInstrs b (INot : acc)
boolToInstrs (BAnd b1 b2)    acc = boolToInstrs b1 $ boolToInstrs b2 $ IAnd : acc

boolToCode :: BExpr -> Code
boolToCode b = boolToInstrs b []

stmtToInstrs :: Stmt -> Code -> Code
stmtToInstrs  SSkip         acc = acc
stmtToInstrs (SWhile b s)   acc = ILoop (boolToCode b) (compile s) : acc
stmtToInstrs (SAssign x a)  acc = aritToInstrs a  $ IStore x : acc
stmtToInstrs (SSeq s1 s2)   acc = stmtToInstrs s1 $ stmtToInstrs s2 acc
stmtToInstrs (SIte b se st) acc = boolToInstrs b  $ IBranch (compile se) (compile st) : acc

compile :: Stmt -> Code
compile s = stmtToInstrs s []

-- Reads the file named on the command line, then parse and compile it
-- Output is to stdout, redirect it to save
main :: IO ()
main = do
  a <- getArgs
  s <- parse a
  t <- case Parser.parse s of
         Right t  -> return t
         Left err -> putStrLn ("Parse error: " ++ err) >> die 2
  putStrLn (showCode True 0 (compile t))

parse :: [String] -> IO String
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [f]    = readFile f
parse _      = usage   >> die 1

usage :: IO ()
usage   = putStrLn "Usage: whilec [-vh] [file]"

version :: IO ()
version = putStrLn "Haskell whilec 0.1"

exit :: IO a
exit    = exitSuccess

die :: Int -> IO a
die i   = exitWith (ExitFailure i)

