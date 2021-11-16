module WhileC (compile, main) where

import System.Environment
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))

import WhileAST
import BamAST
import qualified Parser(parse)

aritToInstrs :: AExpr -> Code -> Code
aritToInstrs = undefined

boolToInstrs :: BExpr -> Code -> Code
boolToInstrs = undefined

boolToCode :: BExpr -> Code
boolToCode b = boolToInstrs b []

stmtToInstrs :: Stmt -> Code -> Code
stmtToInstrs = undefined

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

