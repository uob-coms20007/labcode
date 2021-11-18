module Bam (exec) where

import qualified Data.Map as M
import BamAST

type State = M.Map String Integer

zget :: State -> String -> Integer
zget s x = M.findWithDefault 0 x s

set :: State -> String -> Integer -> State
set s x v = M.insert x v s

-- The abstract machine
data Val = VInt Integer | VBool Bool

instance Show Val where
  show (VInt n)  = show n
  show (VBool b) = show b

type Stack  = [Val]
type Config = (Code, Stack, State)

showConfig :: Config -> String
showConfig (c, e, s) = "(" ++ showFlatCode c ++ "," ++ show e ++ "," ++ show s ++ ")"

evalOp :: Instr -> [Val] -> [Val]
evalOp IAdd (VInt n2  : VInt n1  : e) = VInt  (n1 +  n2) : e
evalOp IMul (VInt n2  : VInt n1  : e) = VInt  (n1 *  n2) : e
evalOp ISub (VInt n2  : VInt n1  : e) = VInt  (n1 -  n2) : e
evalOp INot (VBool b : e)             = VBool (not b) : e
evalOp IAnd (VBool b2 : VBool b1 : e) = VBool (b1 && b2) : e
evalOp IEq  (VInt n2  : VInt n1  : e) = VBool (n1 == n2) : e
evalOp ILe  (VInt n2  : VInt n1  : e) = VBool (n1 <= n2) : e
evalOp _    _                         = undefined

-- Big step
backtrace :: Config -> Either String (Stack, State) -> Either String (Stack, State)
backtrace co (Left m) = Left $ m ++ "\n→ Reached from: " ++ showConfig co
backtrace _  r        = r

execC :: Config -> Either String (Stack, State)
execC ([], e, s)                              = Right (e, s)
execC ([INoop], e, s)                         = Right (e, s)
execC ([IPush (CNum n)], e, s)                = Right (VInt  n : e, s)
execC ([IPush (CBool b)], e, s)               = Right (VBool b : e, s)
execC ([INot], e@(VBool _ : _), s)            = Right (evalOp INot e, s)
execC ([IAnd], e@(VBool _ : VBool _ : _), s)  = Right (evalOp IAnd e, s)
execC ([IAdd], e@(VInt  _ : VInt  _ : _), s)  = Right (evalOp IAdd e, s)
execC ([IMul], e@(VInt  _ : VInt  _ : _), s)  = Right (evalOp IMul e, s)
execC ([ISub], e@(VInt  _ : VInt  _ : _), s)  = Right (evalOp ISub e, s)
execC ([IEq ], e@(VInt  _ : VInt  _ : _), s)  = Right (evalOp IEq  e, s)
execC ([ILe ], e@(VInt  _ : VInt  _ : _), s)  = Right (evalOp ILe  e, s)
execC ([IFetch x] , e, s)                     = Right (VInt (zget s x) : e, s)
execC ([IStore x] , VInt n : e, s)            = Right (e, set s x n)
execC co@([IBranch ct _], VBool True : e, s)  = backtrace co $ execC (ct, e, s)
execC co@([IBranch _ ce], VBool False : e, s) = backtrace co $ execC (ce, e, s)
execC co@([ILoop cc cb], e, s)                =
  case backtrace co $ execC (cc, e, s) of
    Right (VBool b : e1, s1) ->
      if b
      then case backtrace co $ execC (cb, e1, s1) of
             Right (e2, s2) -> backtrace co $ execC ([ILoop cc cb], e2, s2)
             r              -> r
      else Right (e1, s1)
    Right (e1, s1)           ->
      Left $ "Stuck configuration: " ++ showConfig co
          ++ "\n\tCondition `" ++ showFlatCode cc
          ++ "' did not evaluate to a boolean in the initial store."
          ++ "\n\tIt evaluated to the following instead:"
          ++ "\n\t → Stack: " ++ show e1
          ++ "\n\t → Store: " ++ show s1
    r                        -> r
execC co@([_], _, _)                          = Left $ "Stuck configuration: " ++ showConfig co
execC co@(i : c, e, s)                        =
  case backtrace co $ execC ([i], e, s) of
    Right (e1, s1) -> backtrace co $ execC (c, e1, s1)
    r              -> r

exec :: Code -> Either String (Stack, State)
exec c = execC (c, [], M.empty)

