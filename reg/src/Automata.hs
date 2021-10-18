module Automata where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import TrSys

-- The following functions will have their definitions replaced by 
-- /undefined/ in the student version.

-- {-# ANN autoTrSys () #-}
-- {-# ANN member () #-}


-- | /Label a/ is the type of automaton transition labels, where:
--   - /E/ represents an epsilon transition
--   - /S a/ represents a transition on letter /a/

data Label a = E | S a
  deriving (Eq, Ord)

instance Show a => Show (Label a) where
  show E = " "
  show (S a) = show a

-- | /Auto a q/ is the type of automata, represented as a record
-- containing three fields:
--   - /start m/ is the initial state of automaton /m/.
--   - /final m/ is a list of the final states of automaton /m/.
--   - /trans m/ is the list of all transitions of automaton /m/,
--     with each transition represented by a tuple of the form /(q1, l, q2)/

data Auto a q =
  MkAuto {
    start  :: q,
    trans  :: [(q, Label a, q)],
    final  :: [q]
  } deriving (Show)


-- | Given an automaton /m/ and a state /q/ of /m/, /next m q/ is
-- the set of all pairs /(l, p)/ such that /(q, l, p)/ is a transition of /m/.

next :: (Ord q, Ord a) => Auto a q -> q -> Set (Label a, q)
next m q =
    Set.fromList (map (\(_, a, p) -> (a, p)) relevant)
  where
    relevant = filter (\(p, _, _) -> p == q) (trans m)

-- | /Parity/ is the type of states in the odd/even example automaton.

data Parity =
  Odd | Even
  deriving (Show,Eq,Ord)

-- | /ev/ is an automaton recognising the language of binary strings
-- that contain an even number of /1/ characters.

ev :: Auto Char Parity
ev =
  MkAuto {
    start = Even,
    trans = [
        (Even, S '1', Odd),
        (Even, S '0', Even),
        (Odd, S '0', Odd),
        (Odd, S '1', Even)
      ],
    final = [Even]
  }

-- | /zzo/ is an automaton recognising the language of binary strings 
-- that contain the substring /"001"/.

zzo :: Auto Char Int
zzo =
  MkAuto {
    start = 0,
    trans = [
      (0, S '0', 0),
      (0, S '1', 0),
      (0, S '0', 1),
      (1, S '0', 2),
      (2, S '1', 3),
      (3, S '0', 3),
      (3, S '1', 3)
    ],
    final = [3]
  }


-- | A state of the /evzzo/ automaton is either a state of the /ev/ automaton,
-- or a state of the /zzo/ automaton or the new initial state.

data EvzzoState = Ev Parity | Zzo Int | Init
  deriving (Eq, Ord)

instance Show EvzzoState where
  show (Ev p)  = show p
  show (Zzo n) = show n
  show Init    = "Init"

-- | /evzzo/ is an automaton recognising the union of the languages of /ev/ and /zzo/.

evzzo :: Auto Char EvzzoState
evzzo =
  MkAuto {
    start = Init,
    trans =
      [(Init, E, Ev Even), (Init, E, Zzo 0)]
        ++ map (\(p,a,q) -> (Ev p,a,Ev q)) (trans ev)
        ++ map (\(p,a,q) -> (Zzo p,a,Zzo q)) (trans zzo),
    final = map Ev (final ev) ++ map Zzo (final zzo)
  }

-- | Given an automaton /m/, /autoTrSys m/ is the associated transition system,
-- whose configurations are of the form (q, w), where:
--   - /q/ is a state of /m/
--   - /w/ is a word over the alphabet of /m/

autoTrSys :: (Ord a, Ord q) => Auto a q -> TrSys (q, [a])
autoTrSys m (q,w) =
    case w of
      []   -> eSuccs q w
      b:bs -> bSuccs q b bs `Set.union` eSuccs q w
  where
    eSuccs q bs = foldr (insertIfEqTo E bs) Set.empty (next m q)
    bSuccs q b bs = foldr (insertIfEqTo (S b) bs) Set.empty (next m q)
    insertIfEqTo x bs (l,p) cs = if l == x then Set.insert (p,bs) cs else cs

-- | Given an automaton /m/ and a word /w/, /member m w/ just if /w/ is 
-- accepted by /m/.

member :: (Ord a, Ord q) => Auto a q -> [a] -> Bool
member m a =
    not (null fs)
  where
    ss = reachable (autoTrSys m) (Set.singleton (start m, a))
    fs = Set.filter (\(q,w) -> q `elem` final m && null w) ss

-- | Given a __deterministic__ automaton /m/ and a word /w/,
-- /dfaMember m w/ just if /w/ is accepted by /m/.  

dfaMember :: (Ord a, Ord q) => Auto a q -> [a] -> Bool
dfaMember m a = q `elem` final m
  where
    tr = theTrace (autoTrSys m) (start m, a)
    (q,_) = last tr


