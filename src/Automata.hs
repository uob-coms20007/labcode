module Automata where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import TrSys

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


-- | Given an automaton /m/ and a state /q/ of /m/, /successors m q/ is
-- the set of all pairs /(l, p)/ such that /(q, l, p)/ is a transition of /m/.

successors :: (Ord q, Ord a) => Auto a q -> q -> Set (Label a, q)
successors m q =
    Set.fromList (map (\(_, a, p) -> (a, p)) relevant)
  where
    relevant = filter (\(p, _, _) -> p == q) (trans m)

-- | /Parity/ is the type of states in the odd/even example automaton.

data Parity =
  Odd | Even
  deriving (Show,Eq,Ord)

-- | /even/ is an automaton recognising the language of binary strings
-- that contain an even number of /1/ characters.

even :: Auto Char Parity
even =
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

-- | Given an automaton /m/, /autoTrSys m/ is the associated transition system,
-- whose configurations are of the form (q, w), where:
--   - /q/ is a state of /m/
--   - /w/ is a word over the alphabet of /m/

autoTrSys :: (Ord a, Ord q) => Auto a q -> TrSys (q, [a])
autoTrSys m (q,w) =
    Set.union (onSym m (q,w)) (Set.map (\p -> (p,w)) (cl m (Set.singleton q)))
  where
    onSym m (_,[])   = Set.empty
    onSym m (q,b:bs) = Set.map (\p -> (p,bs)) (cl' m bSuccs)
      where
        bSuccs = foldr (\(l,p) ps -> if l == S b then Set.insert p ps else ps) Set.empty (successors m q)

-- | Given an automaton /m/ and a set of states /qs/ of /m/, /cl m qs/ is 
-- the set of states that are reachable from any state in /qs/ by one or 
-- more epsilon transitions.
--
-- NOTE: /cl m qs/ will not necessarily contain /qs/.

cl :: (Ord a, Ord q) => Auto a q -> Set q -> Set q
cl m ss =
    clWithFr ss Set.empty
  where
    clWithFr fs ss =
      let newFr = oneStep fs ss in
        if null newFr then ss else clWithFr newFr (Set.union ss newFr)
    oneStep fs ss =
        foldr (Set.union . newSuccs) Set.empty fs
      where
        -- we only care about those successors that
        -- (a) are reached via an epsilon transition
        -- (b) we haven't seen in any previous iteration
        relevant (l,s) =
          l == E && notElem s ss
        newSuccs f =
          Set.map snd (Set.filter relevant (successors m f))

-- | Given an automaton /m/ and a set of states /qs/, /cl' m qs/ is
-- the set of states reachable from any state in /qs/ by zero or more
-- epsilon transitions.

cl' :: (Ord q, Ord a) => Auto a q -> Set q -> Set q
cl' m ss = Set.union (cl m ss) ss

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


