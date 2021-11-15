module Var (Var, mkVar) where

type Var = String

mkVar :: String -> Var
mkVar s = s

