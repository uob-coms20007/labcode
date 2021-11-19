module BamC (compile, main) where

import BamAST as H
import BlamAST as L
import qualified Parser(parse)

-- `lower` naively lowers a BAM program to the BLAM
-- We're not trying to be fast here (yet), just trying to survive

lower :: Int -> H.Code -> (Int, L.Code)
lower n [] = (n, [])
lower n (IBranch ct ce : is) =
  let (nt, ct') = lower (n + 1) ct in
  let (ne, ce') = lower (nt + 1) ce in
  let (ns, cs)  = lower ne is in
  (ns, (IGotoF (nt + 1) : ct') ++ (IGoto ne : ce') ++ cs)
lower n (ILoop cc cb : is) =
  let (nc, cc') = lower n cc in
  let (nb, cb') = lower (nc + 1) cb in
  let (ns, cs)  = lower (nb + 1) is in
  (ns, cc' ++ (IGotoF (nb + 1) : cb') ++ (IGoto n : cs))
lower n (i : is) =
  let (n', c) = lower (n + 1) is in
  (n', lowerI i : c)
  where
    lowerI :: H.Instr -> L.Instr
    lowerI (H.IPush c)  = L.IPush $ lowerC c
    lowerI  H.IAdd      = L.IAdd
    lowerI  H.IMul      = L.IMul
    lowerI  H.ISub      = L.ISub
    lowerI  H.INot      = L.INot
    lowerI  H.IAnd      = L.IAnd
    lowerI  H.IEq       = L.IEq
    lowerI  H.ILe       = L.ILe
    lowerI (H.IFetch x) = L.IFetch x
    lowerI (H.IStore x) = L.IStore x
    lowerI  H.INoop     = L.INoop
    lowerI  _           = undefined

    lowerC :: H.Const -> L.Const
    lowerC (H.CNum  c) = L.CNum c
    lowerC (H.CBool b) = L.CBool b

-- Attempt 2: a (local) 2-pass version to avoid quadratic behaviour
-- The main issue that stops us happily accumulating is that we need
-- information about the number of lines of code that were produced when
-- compiling a program's prefix when we compile the program's suffix (so the
-- jumps can be aimed right).
-- So we just do a first pass to compute jump targets
lower' :: Int -> H.Code -> L.Code -> L.Code
lower' _ [] acc = acc
lower' n (IBranch ct ce : is) acc =
    -- We do a first pass to compute jumps
    let nt = size ct in
    let ne = size ce in
    -- So we can still accumulate instead of either being quadratic or using a
    -- mutable array like imperative plebs.
    IGotoF (n + nt + 1) : (lower' (n + 1) ct (IGoto (n + nt + ne + 2) : (lower' (n + nt + 2) ce (lower' (n + nt + ne + 2) is acc))))
lower' n (ILoop cc cb : is) acc =
    let nc = size cc in
    let nb = size cb in
    lower' n cc (IGotoF (n + nc + nb + 2) : (lower' (n + nc + 1) cb (IGoto n : lower' (n + nc + nb + 2) is acc)))
lower' n (i : is) acc = lowerI i : lower' (n + 1) is acc
  where
    lowerI :: H.Instr -> L.Instr
    lowerI (H.IPush c)  = L.IPush $ lowerC c
    lowerI  H.IAdd      = L.IAdd
    lowerI  H.IMul      = L.IMul
    lowerI  H.ISub      = L.ISub
    lowerI  H.INot      = L.INot
    lowerI  H.IAnd      = L.IAnd
    lowerI  H.IEq       = L.IEq
    lowerI  H.ILe       = L.ILe
    lowerI (H.IFetch x) = L.IFetch x
    lowerI (H.IStore x) = L.IStore x
    lowerI  H.INoop     = L.INoop
    lowerI  _           = undefined

    lowerC :: H.Const -> L.Const
    lowerC (H.CNum  c) = L.CNum c
    lowerC (H.CBool b) = L.CBool b

size :: H.Code -> Int
size []                   = 0
size (IBranch ct ce : is) = 2 + size ct + size ce + size is
size (ILoop   cc cb : is) = 2 + size cc + size cb + size is
size (_ : is)             = 1 + size is


compile :: H.Code -> L.Code
compile s = snd $ lower 0 s
-- compile s = lower' 0 s []

-- Reads the input off of stdin
-- Output is to stdout, redirect it to save
main :: IO ()
main = do
  s <- getContents
  let t = Parser.parse s
  putStrLn (L.showCode True (compile t))

