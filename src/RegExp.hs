{-# LANGUAGE OverloadedStrings #-}

module RegExp where

import Data.String (IsString,fromString)

import Data.Set (Set)
import qualified Data.Set as Set

import TrSys
import Automata

data RegExp =
  Sym Char | Alt RegExp RegExp | Cat RegExp RegExp | Eps | Rep RegExp

instance IsString RegExp where
  fromString []     = Eps
  fromString (c:cs) = foldl (\e c -> e `Cat` Sym c) (Sym c) cs

showRegExp :: Int -> RegExp -> String
showRegExp pr (Sym a) = [a]
showRegExp pr Eps =
  if pr == 4 then [] else "_"
showRegExp pr (Alt e1 e2) =
  bracket (pr > 2) (showRegExp 2 e1 ++ " | " ++ showRegExp 2 e2)
showRegExp pr (Cat e1 e2) =
  bracket (pr > 4) (showRegExp 4 e1 ++ showRegExp 4 e2)
showRegExp pr (Rep e) = bracket (pr > 5) (showRegExp 6 e ++ "*")

bracket :: Bool -> String -> String
bracket p s = if p then "(" ++ s ++ ")" else s

instance Show RegExp where
  show = showRegExp 0

compile :: RegExp -> Auto Char Int
compile = undefined


