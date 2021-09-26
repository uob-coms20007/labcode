module RegExpTests where

import Test.Tasty
import Test.Tasty.HUnit

import RegExp

matches rex str = assertBool "" (match rex str)
notMatches rex str = assertBool "" (not $ match rex str)

regexpTests =
  testGroup "Regular Expression Tests" [sipserTests, starFreeTests, plLextests]

mkGoodCases cat rex = map (\s -> testCase (cat ++ " Correct " ++ show s) $ matches rex s)
mkBadCases cat rex = map (\s -> testCase (cat ++ " Incorrect " ++ show s) $ notMatches rex s)

sipserTests =
  testGroup "R1: Sipser Tests" $
    mkGoodCases "A" exprA aGood
    ++ mkBadCases "A" exprA aBad
    ++ mkGoodCases "B" exprB bGood
    ++ mkBadCases "B" exprB bBad
    ++ mkGoodCases "C" exprC cGood
    ++ mkBadCases "C" exprC cBad
    ++ mkGoodCases "D" exprD dGood
    ++ mkBadCases "D" exprD dBad
    ++ mkGoodCases "E" exprE eGood
    ++ mkBadCases "E" exprE eBad
  where
    aGood = ["010", "1", "00010", "01000", "001", "1000"]
    aBad  = ["11", "000", "0101", "0110"]
    bGood = ["101000101", "001"]
    bBad  = ["00", "01", "0101010"]
    cGood = ["00", "01", "10", "11", "0000", "0101", "11111111"]
    cBad  = ["0", "1", "010", "00000"]
    dGood = ["0", "", "01", "0111", "111111"]
    dBad  = ["00", "010"]
    eGood = ["0", "1", "01", ""]
    eBad  = ["00", "10", "0101"]

starFreeTests =
  testGroup "R2: Star Free Tests" $
    mkGoodCases "UN" exprUN unGood
    ++ mkBadCases "UN" exprUN unBad
    ++ mkGoodCases "Time" exprTime timeGood
    ++ mkBadCases "Time" exprTime timeBad
    ++ mkGoodCases "IPv4" exprIPv4 ipGood
    ++ mkBadCases "IPv4" exprIPv4 ipBad
    where
      unGood = ["sr17466", "cn19829"]
      unBad = ["s174667","s17466r","sr1746"]
      timeGood = ["23:45", "18:00", "01:14", "02:36"]
      timeBad = ["188:34", "18:010", "1800", "25:00", "23:60"]
      ipGood = ["255.255.255.255", "192.168.0.1", "10.10.10.254"]
      ipBad = ["256.256.256.256", "300.10.10.10", "1.2.3", "200.200"]

plLextests =
  testGroup "R3: PL Lexeme Tests" $
    mkGoodCases "C Ident" cIdentLex cidGood
    ++ mkBadCases "C Ident" cIdentLex cidBad
    ++ mkGoodCases "Int Literal" intLitLex intLitGood
    ++ mkBadCases "Int Literal" intLitLex intLitBad
    ++ mkGoodCases "Float Literal" floatLex floatGood
    ++ mkBadCases "Float Literal" floatLex floatBad
  where
    cidGood    = ["_foo", "foo", "Foo", "bar99", "baz_bar_987"]
    cidBad     = ["", "69Yankee", "9", "jumble-joe"]
    intLitGood = ["2", "123", "010123", "0x34aD2b", "0b1010110"]
    intLitBad  = ["", "ab", "1a", "0x12fgh", "0b10012"]
    floatGood  = ["123.2", "1.0", "0.1", "1e34", "1.4e+23", "1.8E56", "233.4E-3", "3E-3"]
    floatBad   = ["12", "12e", "123E", "12.E", "", "e"]

