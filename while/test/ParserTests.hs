module ParserTests where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Either as Either

import Test.Tasty
import Test.Tasty.HUnit

import WhileAST
import Parser
import Lexer
import While
import WhilePG

areEqual :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
areEqual = assertEqual ""

isTrue :: HasCallStack => Bool -> Assertion
isTrue = assertBool ""

parserTests =
  testGroup "Parsing Tests" [aexprTests,bexprTests,stmtTests,playTests]

aexprTests =
  testGroup "P1: Arithmetic Expressions Parsing Tests" [

    testCase "Parsing simple arithmetic expressions" $
      areEqual (Right (EAdd (EVar "a") (EInt 42), [])) -- Expected
      (parseE . scanTokens $ "a + 42"),                -- Actual

    testCase "Parsing simple arithmetic expressions again" $
      areEqual (Right (ESub (EInt 52) (EVar "xrt"),[])) -- Expected
      (parseE . scanTokens $ "52 - xrt"),               -- Actual

    testCase "Parsing complex arithmetic expressions" $
      areEqual (Right (EAdd (EInt 54) (EMul (EVar "x") (EVar "y")),[])) -- Expected
      (parseE . scanTokens $ "54 + x * y"),

    testCase "Parsing complex arithmetic expressions again" $
      areEqual (Right (EMul (EAdd (EInt 54) (EVar "x")) (EVar "y"),[])) -- Expected
      (parseE . scanTokens $ "(54 + x) * y")
  ]

bexprTests =
  testGroup "P2: Boolean Expressions Parsing Tests" [

    testCase "Parsing simple boolean expressions" $
      areEqual (Right (BBool True,[]))               -- Expected
      (parseB . scanTokens $ "true"),                -- Actual

    testCase "Parsing simple boolean expressions again" $
      areEqual (Right (BAnd (BBool True) (BBool False),[]))   -- Expected
      (parseB . scanTokens $ "true /\\ false"),               -- Actual

    testCase "Parsing complex boolean expressions" $
      areEqual (Right (BAnd (BLe (EVar "n") (EInt 0)) (BNot (BEq (EVar "x") (EInt 12))),[])) -- Expected
      (parseB . scanTokens $ "n <= 0 /\\ !x = 12"),

    testCase "Parsing complex boolean expressions again" $
      areEqual (Right (BAnd (BLe (EVar "n") (EInt 0)) (BAnd (BNot (BEq (EVar "x") (EInt 12))) (BBool False)),[])) -- Expected
      (parseB . scanTokens $ "n <= 0 /\\ (!x = 12 /\\ false)")
  ]

stmtTests =
  testGroup "P3: Statement Parsing Tests" [

    testCase "Parsing simple statements" $
      areEqual (Right (SSeq (SAssign "n" (EAdd (EMul (EVar "n") (EInt 8)) (EInt 12))) SSkip,[]))               -- Expected
      (parseS . scanTokens $ "n := n * 8 + 12"),                -- Actual

    testCase "Parsing simple statements again" $
      areEqual (Right (SSeq (SAssign "n" (EAdd (EMul (EVar "n") (EInt 8)) (EInt 12))) (SSeq (SSeq (SAssign "x" (EAdd (EVar "x") (EInt 1))) SSkip) SSkip),[]))   -- Expected
      (parseS . scanTokens $ "n := n * 8 + 12 { x := x + 1 }"),               -- Actual

    testCase "Parsing complex statements" $
      areEqual (Right (SSeq (SIte (BEq (EVar "x") (EInt 0)) (SAssign "n" (EAdd (EVar "k") (EInt 1))) SSkip) SSkip,[])) -- Expected
      (parseS . scanTokens $ "if x = 0 then n := k + 1 else { }"),

    testCase "Parsing complex statements again" $
      areEqual (Right (SSeq (SWhile (BBool True) (SSeq (SAssign "n" (EAdd (EVar "n") (EInt 2))) (SSeq (SIte (BBool True) SSkip SSkip) SSkip))) SSkip,[])) -- Expected
      (parseS . scanTokens $ "while true { n := n + 2 if true then { } else { }}")

  ]

playTests =
  testGroup "P4: While Playground Tests" [

  testCase "Q1a" $
    areEqual (Just 2)
    (Map.lookup "n" (sos (Either.fromRight SSkip $ parse progA) Map.empty)),

  testCase "Q1b" $
    areEqual (Just 5)
    (Map.lookup "r" (sos (Either.fromRight SSkip $ parse progB) (Map.insert "n" (-5) Map.empty))),

  testCase "Q1c" $
    areEqual (Just 42)
    (Map.lookup "b" (sos (Either.fromRight SSkip $ parse progC) (Map.insert "a" 42 Map.empty))),

  testCase "Q1d" $
    areEqual (Just 1)
    (Map.lookup "r" (sos (Either.fromRight SSkip $ parse progD) (Map.insert "n" 43 Map.empty)))

  ]

