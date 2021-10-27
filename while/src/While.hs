module While where

import Data.Map as Map
import WhileAST

type State = Map Var Integer

-- Semantics - These follow exactly the paper definitions
asem :: AExpr -> State -> Integer
asem (EInt n) _     = n
asem (EVar x) s     = Map.findWithDefault 0 x s
asem (EAdd a1 a2) s = (asem a1 s) + (asem a2 s)
asem (EMul a1 a2) s = (asem a1 s) * (asem a2 s)
asem (ESub a1 a2) s = (asem a1 s) - (asem a2 s)

bsem :: BExpr -> State -> Bool
bsem (BBool b) _    = b
bsem (BEq a1 a2) s  = asem a1 s == asem a2 s
bsem (BLe a1 a2) s  = asem a1 s <= asem a2 s
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

