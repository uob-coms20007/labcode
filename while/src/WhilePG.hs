module WhilePG where

progA :: String
progA = "n := n + 2"

progB :: String
progB = unlines [
  "if (0 <= n)",
  "then r := n",
  "else r := 0 - n"]

progC :: String
progC = unlines [
  "b := a + b",
  "a := b - a",
  "b := b - a"]

progD :: String
progD = unlines [
  "while (!n <= 0) {",
  "  r := n",
  "  n := n - 2",
  "}"]

progE :: String
progE = unlines [
  "while (true) {",
  "  n := n + 1",
  "}"]

progFact :: String
progFact = unlines [
  "if (!0 <= n)",
  "then m := 0 - n",
  "else m := n",
  "r := 1",
  "while (1 <= m) {",
  "  r := r * m",
  "  m := m - 1",
  "}",
  "if (!0 <= n)",
  "then r := 0 - r",
  "else { }"]

