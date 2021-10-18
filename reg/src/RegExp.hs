{-# LANGUAGE OverloadedStrings #-}

module RegExp where

import Data.String (IsString,fromString)

import Data.Set (Set)
import qualified Data.Set as Set

import TrSys
import Automata

-- The following functions will have their definitions replaced by 
-- /undefined/ in the student version.

-- {-# ANN compileEps () #-}
-- {-# ANN compileSym () #-}
-- {-# ANN compileCat () #-}
-- {-# ANN compileStar () #-}
-- {-# ANN alts () #-}
-- {-# ANN exprA () #-}
-- {-# ANN exprB () #-}
-- {-# ANN exprC () #-}
-- {-# ANN exprD () #-}
-- {-# ANN exprE () #-}
-- {-# ANN exprUN () #-}
-- {-# ANN exprTime () #-}
-- {-# ANN exprIPv4 () #-}
-- {-# ANN cIdentLex () #-}
-- {-# ANN intLitLex () #-}
-- {-# ANN floatLex () #-}


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
alts (c:cs) = foldr (\c e -> Sym c `Alt` e) (Sym c) cs

exprA :: RegExp
exprA = (Star "0") `Cat` "1" `Cat` (Star "0")
exprB :: RegExp
exprB = (Star sigma) `Cat` "001" `Cat` (Star sigma)
  where sigma = "0" `Alt` "1"
exprC :: RegExp
exprC = Star (sigma `Cat` sigma)
  where sigma = "0" `Alt` "1"
exprD :: RegExp
exprD = ("0" `Alt` Eps) `Cat` (Star "1")
exprE :: RegExp
exprE = ("0" `Alt` Eps) `Cat` ("1" `Alt` Eps)


-- | /exprUN/ is a regular expression matching Bristol usernames.

exprUN :: RegExp
exprUN = alpha `Cat` alpha `Cat` digit `Cat` digit `Cat` digit `Cat` digit `Cat` digit
  where
    alpha = alts "abcdefghijklmnopqrstuvwxyz"
    digit = alts "0123456789"


-- | /exprTime/ is a regular expression matching times in HH:MM 24hr format.

exprTime :: RegExp
exprTime = hours `Cat` ":" `Cat` mins
  where
    hours = (("0" `Alt` "1") `Cat` ds) `Alt` ("2" `Cat` alts "0123")
    mins = alts "012345" `Cat` ds
    ds = alts "0123456789"


-- | /exprIPv4/ is a regular expression matching IPv4 addresses written in decimal.

exprIPv4 :: RegExp
exprIPv4 = octet `dot` octet `dot` octet `dot` octet
  where
    dot e1 e2 = e1 `Cat` "." `Cat` e2
    d4 = alts "01234"
    d5 = alts "012345"
    d9 = alts "0123456789"
    octet =
      -- 1 or 2 digits
      d9 `Alt` (d9 `Cat` d9)
      -- 3 digits
         `Alt` (
           (("0" `Alt` "1") `Cat` d9 `Cat` d9)
             `Alt` ("2" `Cat` ((d4 `Cat` d9)
             `Alt` ("5" `Cat` d5)))
         )


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
compileSym c n = (n+2, m)
  where
    m =
      MkAuto {
        start = n,
        trans = [(n, S c, n+1)],
        final = [n+1]
      }

compileEps :: Int -> (Int, Auto Char Int)
compileEps n = (n+2, m)
  where
    m =
      MkAuto {
        start = n,
        trans = [(n, E, n+1)],
        final = [n+1]
      }

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
compileCat e1 e2 n0 = (n2, m)
  where
    (n1,m1) = compile e1 n0
    (n2,m2) = compile e2 n1
    [f1] = final m1
    m =
      MkAuto {
        start = start m1,
        trans = (f1, E, start m2) : (trans m1 ++ trans m2),
        final = final m2
      }

compileStar :: RegExp -> Int -> (Int, Auto Char Int)
compileStar e1 n0 = (n1+2, m)
  where
    (n1, m1) = compile e1 n0
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

-- | Given a regular expression /rex/ and a string /s/
-- /match rex s/ is /True/ just if /s/ is a string in
-- the language denoted by /rex/ and /False/ otherwise.

match :: RegExp -> String -> Bool
match rex = member (snd (compile rex 0))


cIdentLex :: RegExp
cIdentLex = (alpha `Alt` "_") `Cat` (Star (alpha `Alt` digits `Alt` "_"))
  where
    alpha = alts (['a'..'z'] ++ ['A' .. 'Z'])
    digits = alts ['0'..'9']

intLitLex :: RegExp
intLitLex =
    plus digit `Alt` ("0x" `Cat` (plus hexit)) `Alt` ("0b" `Cat` (plus bit))
  where
    digit = alts ['0'..'9']
    hexit = digit `Alt` alts ['a'..'f'] `Alt` alts ['A'..'F']
    bit   = "0" `Alt` "1"
    plus e = e `Cat` Star e

floatLex :: RegExp
floatLex =
    (decimal `Cat` "." `Cat` decimal `Cat` (exponent `Alt` Eps))
      `Alt` (decimal `Cat` exponent)
  where
    digit = alts ['0'..'9']
    decimal = plus digit
    exponent = ("e" `Alt` "E") `Cat` ("+" `Alt` "-" `Alt` Eps) `Cat` decimal
    plus e = e `Cat` Star e

