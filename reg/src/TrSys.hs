module TrSys where

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (StdGen)
import qualified System.Random as Random

-- The following function definitions will be automatically replaced with 
-- /undefined/ in the student version of the repository.

-- {-# ANN reachable () #-}
-- {-# ANN freachable () #-}
-- {-# ANN rngTrace () #-}
-- {-# ANN chameleons () #-}
-- {-# ANN circuit () #-} 

-- | /TrSys a/ is the type of transition systems over configurations of
-- type /a/.  A value of this type is just a function in /a -> Set a/
-- which yields the set of successors of any configuration (those configurations
-- that are reached from the given one in exactly one step). 

type TrSys a = a -> Set a

-- | Given an ordered type /a/, a transition system /tr/ over /a/ and
-- a configuration /c/ of the system, /reachable' tr c/ is the set of
-- all configurations that are reachable from /c/ according to /tr/.

reachable' :: Ord a => TrSys a -> a -> Set a
reachable' tr c = reachable tr (Set.singleton c)

-- | Given an ordered type /a/, a transition system /tr/ over /a/ 
-- and a set of configurations /cs/, /reachable tr cs/ is the set of
-- of all configurations that are reachable from /cs/ according to /tr/.

reachable :: Ord a => TrSys a -> Set a -> Set a
reachable tr cs =
  reach cs Set.empty

  where
    reach cs ds =
      if cs /= ds then
        reach (cs `Set.union` succs cs) cs
      else cs

    -- /succs cs/ is the set /{d | there is c in cs and c => d}/
    succs =
      Set.unions . Set.map tr

-- | Given an ordered type /a/, a transition system /tr/ over /a/ 
-- and a set of configurations /cs/, /freachable tr cs/ is the set of
-- of all configurations that are reachable from /cs/ according to /tr/.
-- Internally maintains a frontier for efficiency.

freachable :: Ord a => TrSys a -> Set a -> Set a
freachable tr cs =
    reach cs cs

  where

    -- Given a set of frontier configurations /fr/ and the set /acc/ of 
    -- all configurations accumulated so far, /reach fr acc/ is the set 
    -- of configurations consisting of /acc/ and all those configurations
    -- reachable from /fr/.
    reach fr acc =
      let newFr = oneStep fr acc in
        if null newFr then
          acc
        else
          reach newFr (foldr Set.insert acc newFr)

    -- Given a set of /frontier/ configurations /fr/ and the set /acc/ of
    -- all configurations accumulated so far, /oneStep fr acc/ is the set of 
    -- those configurations not already in /acc/ that can be reached from a 
    -- configuration in /fr/ in exactly one step.
    oneStep fr acc =
        foldr (Set.union . newSuccs) Set.empty fr
      where
        newSuccs s =
          Set.filter (`notElem` acc) (tr s)

-- | Given a __deterministic__ transition system /tr/ and a configuration /c/, 
-- /theTrace tr c/ is the (possibly infinite) list of configurations that 
-- comprise the unique trace of /tr/ starting from /c/.

theTrace :: TrSys a -> a -> [a]
theTrace tr c =
    c : List.unfoldr next c
  where
    next c =
      let cs = tr c in
        if null cs then Nothing else Just (Set.findMax cs, Set.findMax cs)

-- | Given an integer /r/, a transition system /tr/ and configuration /c/,
-- /rngTrace r tr c/ is the unique trace of /tr/ that starts in /c/ and 
-- whose choice of transition during computation is determined by the pseudo-random 
-- numbers generated from seed /r/.

rngTrace :: Int -> TrSys a -> a -> [a]
rngTrace r tr c =
    c : List.unfoldr pick (c, gen)
  where
    gen = Random.mkStdGen r
    -- Given a configuration /c/ and the current state of the generator /c/
    -- /pick (c, g)/ is either: 
    --   - /Nothing/ if /c/ is a terminal configuration
    --   - or is of shape /Just (c', (c', g'))/ where:
    --       - /c'/ is a randomly chosen (according to /g/) successor of /c/
    --       - /g'/ is the new state of the random number generator
    pick (c, g) =
      let cs = tr c in
      if null cs then
        Nothing
      else
        let
          (n, g') = Random.uniformR (0, Set.size cs - 1) g
          c' = Set.elemAt n cs
        in
          Just (c', (c', g'))

-- | /chameleons/ is the transition system for the /chameleons problem/.
-- A configuration of this system is represented by a triple of natural
-- numbers /(n,m,p)/ with:
--   - /n/ the number of red chameleons
--   - /m/ the number of green chameleons
--   - /p/ the number of blue chameleons
--
-- In practice we use the /Int/ type and handle the presence of -ve numbers
-- explicitly in the function.

chameleons :: TrSys (Int, Int, Int)
chameleons (n,m,p) =
  Set.fromList (
    (if n > 0 && m > 0 then [(n-1,m-1,p+2)] else [])
    ++ (if n > 0 && p > 0 then [(n-1,m+2,p-1)] else [])
    ++ (if m > 0 && p > 0 then [(n+2,m-1,p-1)] else [])
  )

-- | /Light/ is the type of UK traffic light configurations.

data Light =
    Red | Amber | Green | RedAndAmber
  deriving (Eq, Ord, Show)

-- | /traffic/ is the transition system for the UK traffic light system.
-- A configuration of this system is represented by a value of type /Light/.

traffic :: TrSys Light
traffic x = Set.singleton (tfic x)
  where
    tfic Red = RedAndAmber
    tfic Amber = Red
    tfic Green = Amber
    tfic RedAndAmber = Green

-- | /circuit/ is the transition system for the digital counter circuit.
-- A configuration of this circuit is a quadruple of bits /(x,q0,q1,y)/ 
-- giving the values of the wires and registers on each clock cycle.
--
-- In practice we use the type /Int/ to represent bits, ignoring values
-- different from /0/ and /1/.

circuit :: TrSys (Int, Int, Int, Int)
circuit (0,0,0,_)   = Set.fromList [(0,0,1,0), (1,0,1,0)]
circuit (0,0,1,_)   = Set.fromList [(0,1,0,0), (1,1,0,0)]
circuit (0,1,0,_)   = Set.fromList [(0,1,1,0), (1,1,1,0)]
circuit (_,1,1,_)   = Set.fromList [(0,0,0,1), (1,0,0,1)]
circuit (1,q1,q0,_) = Set.fromList [(0,0,0,0), (1,0,0,0)]

-- | /prog/ is the transition system for the While program example.
-- A configuration of this system is a triple of integers /(pc ,x, y)/ with:
--   /pc/ the value of the program counter (which line of code to exec)
--   /x/  the current value of variable x
--   /y/  the current value of variable y

prog :: TrSys (Int,Int,Int)
prog (1,x,y) = Set.singleton (2,x*x+1,y)
prog (2,x,y) = Set.singleton (if x /= 0 then (3,x,y) else (5,x,y))
prog (3,x,y) = Set.singleton (4,x-2,y)
prog (4,x,y) = Set.singleton (2,x,2*y-1)
prog _       = Set.empty



