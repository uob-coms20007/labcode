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
compile e = snd (compile' e 0)

compile' :: RegExp -> Int -> (Int, Auto Char Int)
compile' (Sym c) n = (n+2, m)
  where
    m = MkAuto { start = n, trans = [(n, S c, n+1)], final = [n+1] }
compile' Eps n = (n+2, m)
  where
    m = MkAuto { start = n, trans = [(n, E, n+1)], final = [n+1] }
compile' (Alt e1 e2) n0 = (n2+3, m)
  where
    (n1,m1) = compile' e1 n0
    (n2,m2) = compile' e2 n1
    [f1] = final m1
    [f2] = final m2
    newTrans =
      [
        (n2+1, E, start m1),
        (n2+1, E, start m2),
        (f1, E, n2+2),
        (f2, E, n2+2)
      ]
    m =
      MkAuto {
        start = n2+1,
        trans = newTrans ++ trans m1 ++ trans m2,
        final = [n2+2]
      }
compile' (Cat e1 e2) n0 = (n2, m)
  where
    (n1,m1) = compile' e1 n0
    (n2,m2) = compile' e2 n1
    [f1] = final m1
    m =
      MkAuto {
        start = start m1,
        trans = (f1, E, start m2) : (trans m1 ++ trans m2),
        final = final m2
      }
compile' (Rep e1) n0 = (n1+1, m)
  where
    (n1, m1) = compile' e1 n0
    [f1] = final m1
    newTrans = [
        (start m1, E, n1+1),
        (f1, E, n1+1),
        (n1+1, E, start m1)
      ]
    m =
      MkAuto {
        start = start m1,
        trans = newTrans ++ trans m1,
        final = [n1+1]
      }


