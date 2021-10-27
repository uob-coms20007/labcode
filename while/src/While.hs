module While where

import System.Environment
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))

import Data.Map as Map
import WhileAST
import qualified Parser(parse)

type State = Map Var Integer

-- Semantics - These follow exactly the paper definitions
asem :: AExpr -> State -> Integer
asem (EInt n) _     = n
asem (EVar x) s     = Map.findWithDefault 0 x s
asem (EBinOp AAdd a1 a2) s = (asem a1 s) + (asem a2 s)
asem (EBinOp AMul a1 a2) s = (asem a1 s) * (asem a2 s)
asem (EBinOp ASub a1 a2) s = (asem a1 s) - (asem a2 s)

bsem :: BExpr -> State -> Bool
bsem (BBool b) _    = b
bsem (BComp AEq a1 a2) s  = asem a1 s == asem a2 s
bsem (BComp ALe a1 a2) s  = asem a1 s <= asem a2 s
bsem (BNot b) s     = not $ bsem b s
bsem (BAnd b1 b2) s = bsem b1 s && bsem b2 s


-- Semantics (Small-Step) - We use Either to avoid having to define an isomorphic type
-- It may be cleaner (and more Haskell-y) to define a type for configurations.
-- It would certainly be more extensible (say, if we wanted to denote stuck
-- configurations)
step :: Stmt -> State -> Either (Stmt, State) State
step (SAssign x e) s  = Right $ Map.insert x (asem e s) s
step (SSeq c1 c2) s   =
  case step c1 s of
    Left (c1', s') -> Left (SSeq c1' c2, s')
    Right s'       -> Left (c2, s')
step (SIte b ct ce) s = Left (if bsem b s then ct else ce, s)
step (SWhile b c) s   = Left (SIte b (SSeq c (SWhile b c)) SSkip, s)
step SSkip s          = Right s

-- This iterates the step function until we reach a terminal configuration
sos :: Stmt -> State -> State
sos c s =
  case step c s of
    Left (c', s') -> sos c' s'
    Right s'      -> s'

main :: IO ()
main = do
  a <- getArgs
  s <- parse a
  t <- case Parser.parse s of
         Right t  -> return t
         Left err -> putStrLn ("Parse error: " ++ err) >> die 2
  putStr (show $ sos t Map.empty)

parse :: [String] -> IO String
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [f]    = readFile f
parse _      = usage   >> die 1

usage :: IO ()
usage   = putStrLn "Usage: while [-vh] [file]"

version :: IO ()
version = putStrLn "Haskell while 0.1"

exit :: IO a
exit    = exitSuccess

die :: Int -> IO a
die i   = exitWith (ExitFailure i)

