module Main where

import System.IO (isEOF)

import Parser
import State
import Bam

run :: String -> IO ()
run inp = do
--  hPutStrLn stderr $ "Parsing string\n" ++ inp ++ "\n"
  let c = parse inp
--  hPutStr stderr $ show c
  case exec c empty of
    Left m       -> do
      putStrLn m
    Right (_, s) -> do
      putStrLn $ "Final Store: " ++ show s

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

