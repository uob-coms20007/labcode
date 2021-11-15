module State (State, empty, zget, mget, get, set) where

import qualified Data.Map as M

import Var

type State = M.Map Var Integer

empty :: State
empty = M.empty

zget :: State -> Var -> Integer
zget s x = M.findWithDefault 0 x s

mget :: State -> Var -> Maybe Integer
mget s x = s M.!? x

get :: State -> Var -> Integer
get s x = case mget s x of Just y -> y; Nothing -> error $ "Variable " ++ x ++ " is not defined in state " ++ show s

set :: State -> Var -> Integer -> State
set s x v = M.insert x v s

