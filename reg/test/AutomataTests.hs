module AutomataTests where

import Data.Set (fromList)
import Data.List (intersect)

import Test.Tasty
import Test.Tasty.HUnit

import TrSys
import Automata

areEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
areEqual = assertEqual ""

automataTests =
  testGroup "Automata Tests" [autoTrSysTests, memberTests]

autoTrSysTests =
  testGroup "A1: Automaton Transition System Tests" [

    testCase "ev reachability test (Even, \"0101\")" $
      areEqual
        (fromList [(Odd,"01"),(Odd,"1"),(Even,""),(Even,"0101"),(Even,"101")])
        (reachable' (autoTrSys ev) (Even, "0101")),

    testCase "ev reachability test (Even, \"\")" $
      areEqual
        (fromList [(Even,"")])
        (reachable' (autoTrSys ev) (Even, "")),

    testCase "zzo reachability test (0, \"01010\")" $
      areEqual
        (fromList [(0,""),(0,"0"),(0,"010"),(0,"01010"),(0,"10"),(0,"1010"),(1,""),(1,"10"),(1,"1010")])
        (reachable' (autoTrSys zzo) (0, "01010")),

    testCase "zzo reachability test (0, \"00010\")" $
      areEqual
        (fromList [(0,""),(0,"0"),(0,"00010"),(0,"0010"),(0,"010"),(0,"10"),(1,""),(1,"0010"),(1,"010"),(1,"10"),(2,"010"),(2,"10"),(3,""),(3,"0")])
        (reachable' (autoTrSys zzo) (0, "00010")),

    testCase "evzzo reachability test (Init, \"001\")" $
      areEqual
        (fromList [(Ev Odd,""),(Ev Even,"001"),(Ev Even,"01"),(Ev Even,"1"),(Zzo 0,""),(Zzo 0,"001"),(Zzo 0,"01"),(Zzo 0,"1"),(Zzo 1,"01"),(Zzo 1,"1"),(Zzo 2,"1"),(Zzo 3,""),(Init,"001")])
        (reachable' (autoTrSys evzzo) (Init, "001"))

  ]

mkGoodCases cat m =
  map (\s -> testCase (cat ++ " Correct " ++ s) $ assertBool "" $ member m s)
mkBadCases cat m =
  map (\s -> testCase (cat ++ " Incorrect " ++ s) $ assertBool "" $ not $ member m s)

memberTests =
  testGroup "A2: Membership Tests" $
    mkGoodCases "ev" ev evGood
    ++ mkBadCases "ev" ev evBad
    ++ mkGoodCases "zzo" zzo zzoGood
    ++ mkBadCases "zzo" zzo zzoBad
    ++ mkGoodCases "evzzo" evzzo evzzoGood
    ++ mkBadCases "evzzo" evzzo evzzoBad
  where
    evGood    = ["", "0", "11", "1111", "0110", "01010101", "000110011"]
    evBad     = ["1", "010", "111", "010101"]
    zzoGood   = ["101000101", "001"]
    zzoBad    = ["1", "010", "111", "010101", "00", "01"]
    evzzoGood = evGood ++ zzoGood
    evzzoBad  = evBad `intersect` zzoBad

