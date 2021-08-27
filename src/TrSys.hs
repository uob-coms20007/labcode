module TrSys where

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import System.Random (StdGen)
import qualified System.Random as Random

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
reachable = undefined

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
rngTrace = undefined

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
chameleons = undefined

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
circuit = undefined

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



