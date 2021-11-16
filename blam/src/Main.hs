module Main where

import System.IO (isEOF)

import Parser
import Blam

run :: String -> IO ()
run inp = do
  let c = parse inp
  let (rtr, r) = exec c
  let tr = unlines (map ((\xs -> '⇒' : ' ' : xs) . showConfig) (reverse rtr))
  case r of
    Left m       -> do
      putStrLn m
      putStrLn $ "Trace: " ++ tr
    Right (_, s) -> do
      putStrLn $ "Final Store: " ++ show s
      putStrLn $ "Trace: " ++ tr

loop :: String -> IO ()
loop inp = do
  done <- isEOF
  if done
  then run inp
  else do
    line <- getLine
    loop $ inp ++ "\n" ++ line
  return ()

main :: IO ()
main = loop ""

