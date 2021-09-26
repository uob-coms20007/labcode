module TrSysTests where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

import TrSys

areEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
areEqual = assertEqual ""

isTrue :: HasCallStack => Bool -> Assertion
isTrue = assertBool ""

trSysTests =
  testGroup "Transition System Tests" [traceTests, rngTraceTests, reachabilityTests]

traceTests =
  testGroup "T1: Trace Tests" [

    testCase "Traffic light sequence from Red" $
      areEqual
        [Red, RedAndAmber, Green, Amber, Red]   -- Expected
        (take 5 (theTrace traffic Red)),        -- Actual 

    testCase "While program with x=1, y=2" $
      areEqual
        [(1,1,2),(2,2,2),(3,2,2),(4,0,2),(2,0,3),(5,0,3)]  -- Expected
        (theTrace prog (1,1,2)),                            -- Actual

    testCase "While program with x=2, y=8 for 8 steps" $
      areEqual
        [                                 -- Expected
          (1,2,8),
          (2,5,8),
          (3,5,8),
          (4,3,8),
          (2,3,15),
          (3,3,15),
          (4,1,15),
          (2,1,29)
        ]
        (take 8 $ theTrace prog (1,2,8))  -- Actual 
  ]

rngTraceTests =
  testGroup "T2: Random Trace Tests" [

    testCase "Total chameleon number invariant" $
      isTrue (all (\(r,b,g) -> r + b + g == 9) $ take 10 $ rngTrace 33 chameleons (2,3,4)),

    testCase "Chameleon differences modulo are invariant" $
      let
        mod3 x =
            if r < 0 then r + 3 else r
              where r = x `mod` 3
        inv (n,m,p) (r,b,g) =
          mod3 (r-b) == n && mod3 (b-g) == m && mod3 (r-g) == p
      in
        isTrue (all (inv (mod3 (4-8), mod3 (8-1), mod3 (4-1))) (take 10 $ rngTrace 67 chameleons (4,8,1))),

    testCase "Randomness" $
      let
        traces = map (\s -> take 5 $ rngTrace s chameleons (10,10,10)) [1..10]
      in
        isTrue (length (List.nub traces) > 1),

    testCase "Output only at reset" $
      let
        bad (x,q1,q0,y) = y == 1 && (q1 /= 0 || q0 /= 0)
      in
        isTrue (not (any bad (take 100 $ rngTrace 4 circuit (0,0,0,0))))

  ]

reachabilityTests =
  testGroup "T3: Reachability Tests" [

    testCase "Traffic from a green light" $
      areEqual
        (Set.fromList [Red, RedAndAmber, Green, Amber])  -- Expected
        (reachable' traffic Green),                      -- Actual

    testCase "Circuit from X=0 and Y=0" $
      areEqual
        (Set.fromList [
            (0,0,0,0),
            (0,0,0,1),
            (0,0,1,0),
            (0,1,0,0),
            (0,1,1,0),
            (1,0,0,0),
            (1,0,0,1),
            (1,0,1,0),
            (1,1,0,0),
            (1,1,1,0)
          ])                             -- Expected
        (reachable' circuit (0,0,0,0)),  -- Actual

    testCase "Chameleons from 2 red, 3 green and 2 blue" $
      areEqual
        (Set.fromList [                 -- Expected
          (0,1,6),
          (0,4,3),
          (0,7,0),
          (1,2,4),
          (1,5,1),
          (2,0,5),
          (2,3,2),
          (3,1,3),
          (3,4,0),
          (4,2,1),
          (5,0,2),
          (6,1,0)
        ])
        (reachable' chameleons (2,3,2)), -- Actual

    testCase "Program from x=3, y=3" $
      areEqual
        (Set.fromList [            -- Expected
          (1,3,3),
          (2,0,65),
          (2,2,33),
          (2,4,17),
          (2,6,9),
          (2,8,5),
          (2,10,3),
          (3,2,33),
          (3,4,17),
          (3,6,9),
          (3,8,5),
          (3,10,3),
          (4,0,33),
          (4,2,17),
          (4,4,9),
          (4,6,5),
          (4,8,3),
          (5,0,65)
        ])
        (reachable' prog (1,3,3))  -- Actual
  ]

