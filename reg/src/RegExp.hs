{-# LANGUAGE OverloadedStrings #-}

module RegExp where

import Data.String (IsString,fromString)

import Data.Set (Set)
import qualified Data.Set as Set

import TrSys
import Automata


-- | /RegExp/ is the type of non-empty regular expressions.

data RegExp =
  Sym Char | Alt RegExp RegExp | Cat RegExp RegExp | Eps | Star RegExp

instance IsString RegExp where
  fromString []     = Eps
  fromString (c:cs) = foldl (\e c -> e `Cat` Sym c) (Sym c) cs

-- | Given a regular expression /r/, /showRegExp r/ is its concrete syntax.

showRegExp :: RegExp -> String
showRegExp = showRegExpP 0
  where
    -- Given a natural number /n/ and a regular expression /r/
    -- /showRegExp n r/ is the string representation of /r/ appropriately
    -- bracketed for insertion into a context of precedence /n/.
    showRegExpP :: Int -> RegExp -> String
    showRegExpP pr (Sym a) = [a]
    showRegExpP pr Eps =
      if pr == 4 then [] else "_"
    showRegExpP pr (Alt e1 e2) =
      bracket (pr > 2) (showRegExpP 2 e1 ++ " | " ++ showRegExpP 2 e2)
    showRegExpP pr (Cat e1 e2) =
      bracket (pr > 4) (showRegExpP 4 e1 ++ showRegExpP 4 e2)
    showRegExpP pr (Star e) = bracket (pr > 5) (showRegExpP 6 e ++ "*")

    bracket :: Bool -> String -> String
    bracket p s = if p then "(" ++ s ++ ")" else s

instance Show RegExp where
  show = showRegExp

-- | Given a *non-empty* list of characters /[c1,c2,...,cn]/,
-- /alt [c1,c2,...,cn]/ is /c1 `Alt` c2 `Alt` ... `Alt` cn/.

alts :: String -> RegExp
alts = undefined

exprA :: RegExp
exprA = undefined
exprB :: RegExp
exprB = undefined
exprC :: RegExp
exprC = undefined
exprD :: RegExp
exprD = undefined
exprE :: RegExp
exprE = undefined


-- | /exprUN/ is a regular expression matching Bristol usernames.

exprUN :: RegExp
exprUN = undefined


-- | /exprTime/ is a regular expression matching times in HH:MM 24hr format.

exprTime :: RegExp
exprTime = undefined


-- | /exprIPv4/ is a regular expression matching IPv4 addresses written in decimal.

exprIPv4 :: RegExp
exprIPv4 = undefined


-- | Given a regular expression /rex/ and a natural number /n/,
-- /compileAlt e n/ is a pair /(p,m)/ where /m/ is an NFA with
-- the following properties:
--   * /m/ recognises the language denoted by /rex/
--   * the states of /m/ are numbers in the range /n/--/p-1/
--   * /m/ has exactly one final state

compile :: RegExp -> Int -> (Int, Auto Char Int)
compile Eps n         = compileEps n
compile (Sym c) n     = compileSym c n
compile (Alt e1 e2) n = compileAlt e1 e2 n
compile (Cat e1 e2) n = compileCat e1 e2 n
compile (Star e) n    = compileStar e n

compileSym :: Char -> Int -> (Int, Auto Char Int)
compileSym = undefined

compileEps :: Int -> (Int, Auto Char Int)
compileEps = undefined

-- | Given regular expressions /e1/ and /e2/ and a number /n/,
-- /compileAlt e1 e2 n/ is a pair /(n',m)/ where /m/ is an NFA
-- equivalent to /Alt e1 e2/ whose states take names in the 
-- range /n/ -- /n'-1/ (inclusive).
compileAlt :: RegExp -> RegExp -> Int -> (Int, Auto Char Int)
compileAlt e1 e2 n0 = (n2+3, m)
  where
    -- Recursively compute automata
    -- corresponding to /e1/ and /e2/.
    (n1,m1) = compile e1 n0
    (n2,m2) = compile e2 n1
    -- The compile function guarantees
    -- that the NFAs produced have exactly
    -- one final state.
    [f1] = final m1
    [f2] = final m2
    -- To create an NFA that recognises
    -- /Alt e1 e2/ we "create" a new start
    -- state using the next available number,
    -- which is /n2+1/ and then hook it up
    -- to the start states of /m1/ and /m2/
    -- using epsilon transitions in /newTrans/.
    newTrans =
      [
        (n2+1, E, start m1),
        (n2+1, E, start m2),
        -- To maintain the data structure
        -- invariant that the automaton
        -- produced has exactly one final
        -- state, we create a new final 
        -- state and hook it up to the final
        -- states of m1 and m2 using epsilon
        -- transitions.
        (f1, E, n2+2),
        (f2, E, n2+2)
      ]
    -- The NFA has all the transitions of the NFAs
    -- corresponding to /e1/ and /e2/ and additionally
    -- it has the epsilon transitions from /newTrans/.
    m =
      MkAuto {
        start = n2+1,
        trans = newTrans ++ trans m1 ++ trans m2,
        final = [n2+2]
      }

compileCat :: RegExp -> RegExp -> Int -> (Int, Auto Char Int)
compileCat = undefined

compileStar :: RegExp -> Int -> (Int, Auto Char Int)
compileStar = undefined

-- | Given a regular expression /rex/ and a string /s/
-- /match rex s/ is /True/ just if /s/ is a string in
-- the language denoted by /rex/ and /False/ otherwise.

match :: RegExp -> String -> Bool
match rex = member (snd (compile rex 0))


cIdentLex :: RegExp
cIdentLex = undefined

intLitLex :: RegExp
intLitLex = undefined

floatLex :: RegExp
floatLex = undefined

