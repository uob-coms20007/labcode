module BamC (compile, main) where

import BamAST as H
import BlamAST as L
import qualified Parser(parse)

-- `lower` naively lowers a BAM program to the BLAM
-- We're not trying to be fast here (yet), just trying to survive

lower :: Int -> H.Code -> (Int, L.Code)
lower = undefined

compile :: H.Code -> L.Code
compile s = snd $ lower 0 s

-- Reads the file named on the command line, then parse and compile it
-- Output is to stdout, redirect it to save
main :: IO ()
main = do
  s <- getContents
  let t = Parser.parse s
  putStrLn (L.showCode True (compile t))

