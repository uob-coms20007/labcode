module TrSysTests where

import Data.Set (Set)
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import TrSys

areEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
areEqual = assertEqual ""

trSysTests =
  testGroup "Transition System Tests" [traceTests, reachabilityTests]

traceTests =
  testGroup "Trace Tests" [

    testCase "Traffic light sequence from Red" $
      areEqual 
        (take 5 (theTrace traffic Red))        -- Actual 
        [Red, RedAndAmber, Green, Amber, Red], -- Expected

    testCase "While program with x=1, y=2" $
      areEqual
        (theTrace prog (1,1,2))                            -- Actual
        [(1,1,2),(2,2,2),(3,2,2),(4,0,2),(2,0,3),(5,0,3)], -- Expected

    testCase "While program with x=2, y=8 for 8 steps" $
      areEqual 
        (take 8 $ theTrace prog (1,2,8))  -- Actual
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
  ]

reachabilityTests =
  testGroup "Reachability Tests" [

    testCase "Traffic from a green light" $
      areEqual 
        (reachable' traffic Green)                       -- Actual
        (Set.fromList [Red, RedAndAmber, Green, Amber]), -- Expected

    testCase "Circuit from X=0 and Y=0" $ 
      areEqual 
        (reachable' circuit (0,0,0,0))  -- Actual
        (Set.fromList [                 -- Expected
          (0,0,0,0),
          (0,0,1,0),
          (0,1,0,0),
          (0,1,1,1),
          (1,0,0,0),
          (1,0,1,0),
          (1,1,0,0),
          (1,1,1,1)
        ]),

    testCase "Chameleons from 2 red, 3 green and 2 blue" $
      areEqual 
        (reachable' chameleons (2,3,2)) -- Actual
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
        ]),

    testCase "Program from x=3, y=3" $
      areEqual 
        (reachable' prog (1,3,3))  -- Actual
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
  ]