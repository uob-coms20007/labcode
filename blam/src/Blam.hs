module Blam (exec, showConfig) where

import Data.List(foldl')
import qualified Data.Map as M

import BlamAST

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
type Config = (Code, Code, Stack, State)

showConfig :: Config -> String
showConfig (pre, c : suf, e, s) = "(" ++ showFlatCode (reverse pre) ++ " > " ++ showFlatCode [c] ++ " < " ++ showFlatCode suf ++ "," ++ show e ++ "," ++ show s ++ ")"
showConfig (pre, [], e, s)      = "(" ++ showFlatCode (reverse pre) ++ " >  < ," ++ show e ++ "," ++ show s ++ ")"

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
execC :: Config -> [Config] -> ([Config], Either String (Stack, State))
execC co@(_, [], e, s)                                     tr = (co : tr, Right (e, s))
execC co@(pre, c@INoop : suf, e, s)                        tr = execC (c : pre, suf, e, s) (co : tr)
execC co@(pre, c@(IPush (CNum n)) : suf, e, s)             tr = execC (c : pre, suf, VInt n : e, s) (co : tr)
execC co@(pre, c@(IPush (CBool b)) : suf, e, s)            tr = execC (c : pre, suf, VBool b : e, s) (co : tr)
execC co@(pre, c@INot : suf, e@(VBool _ : _), s)           tr = execC (c : pre, suf, evalOp INot e, s) (co : tr)
execC co@(pre, c@IAnd : suf, e@(VBool _ : VBool _ : _), s) tr = execC (c : pre, suf, evalOp IAnd e, s) (co : tr)
execC co@(pre, c@IAdd : suf, e@(VInt  _ : VInt  _ : _), s) tr = execC (c : pre, suf, evalOp IAdd e, s) (co : tr)
execC co@(pre, c@IMul : suf, e@(VInt  _ : VInt  _ : _), s) tr = execC (c : pre, suf, evalOp IMul e, s) (co : tr)
execC co@(pre, c@ISub : suf, e@(VInt  _ : VInt  _ : _), s) tr = execC (c : pre, suf, evalOp ISub e, s) (co : tr)
execC co@(pre, c@IEq  : suf, e@(VInt  _ : VInt  _ : _), s) tr = execC (c : pre, suf, evalOp IEq  e, s) (co : tr)
execC co@(pre, c@ILe  : suf, e@(VInt  _ : VInt  _ : _), s) tr = execC (c : pre, suf, evalOp ILe  e, s) (co : tr)
execC co@(pre, c@(IFetch x) : suf, e, s)                   tr = execC (c : pre, suf, VInt (zget s x) : e, s) (co : tr)
execC co@(pre, c@(IStore x) : suf, VInt n : e, s)          tr = execC (c : pre, suf, e, set s x n) (co : tr)
execC co@(pre, c@(IGoto  l) : suf, e, s)                   tr
  | 0 <= l && l < length pre =
      let (ppre, spre) = splitAt (length pre - l) pre in
      execC (spre, foldl' (flip (:)) (c : suf) ppre, e, s) (co : tr)
  | length pre <= l && l <= length pre + 1 + length suf =
      let (psuf, ssuf) = splitAt (l - length pre) (c : suf) in
      execC (foldl' (flip (:)) pre psuf, ssuf, e, s) (co : tr)
execC co@(pre, c@(IGotoF _) : suf, VBool True : e, s)      tr = execC (c : pre, suf, e, s) (co : tr)
execC co@(pre, c@(IGotoF l) : suf, VBool False : e, s)     tr
  | 0 <= l && l < length pre =
      let (ppre, spre) = splitAt (length pre - l) pre in
      execC (spre, foldl' (flip (:)) (c : suf) ppre, e, s) (co : tr)
  | length pre <= l && l <= length pre + 1 + length suf =
      let (psuf, ssuf) = splitAt (l - length pre) (c : suf) in
      execC (foldl' (flip (:)) pre psuf, ssuf, e, s) (co : tr)
execC co                                                    tr = (co : tr, Left $ "Stuck configuration: " ++ showConfig co)

exec :: Code -> ([Config], Either String (Stack, State))
exec c = execC ([], c, [], M.empty) []

