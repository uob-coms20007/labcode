module NonDet where

import Data.List ( (\\), intersperse )

-- {-# ANN sendMoreMoney () #-}

-- | /guard b/ is /[()]/ if /b/ is /True/ and /[]/ if /b/ is /False/.

guard :: Bool -> [()]
guard True = [()]
guard False = []

-- | /digits/ is a list of all decimal digits

digits :: [Int]
digits = [0..9]

-- | /bools/ is a list of all the Booleans

bools :: [Bool]
bools = [True, False]

-- | /prop/ is a list of all satisfying assignments 
-- (p,q,r,s,t) to the formula /(p && (q => r) || ¬s) && ¬t/

prop :: [(Bool,Bool,Bool,Bool,Bool)]
prop =
  do p <- bools
     q <- bools
     r <- bools
     s <- bools
     t <- bools
     guard ((p && (q `implies` r) || not s) && not t)
     return (p,q,r,s,t)
  where
    True `implies` False = False
    _    `implies` _     = True


-- | /sendMoreMoney/ is a list of all solutions to the SENDMOREMONEY problem

sendMoreMoney :: [(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)]
sendMoreMoney =
  do s <- digits
     e <- digits \\ [s]
     n <- digits \\ [s,e]
     d <- digits \\ [s,e,n]
     m <- digits \\ [s,e,n,d]
     o <- digits \\ [s,e,n,d,m]
     r <- digits \\ [s,e,n,d,m,o]
     y <- digits \\ [s,e,n,d,m,o,r]
     guard (y == (d + e) `mod` 10)
     let c1 = (d + e) `quot` 10
     guard (e == (n + r + c1) `mod` 10)
     let c2 = (n + r + c1) `quot` 10
     guard (n == (e + o + c2) `mod` 10)
     let c3 = (e + o + c2) `quot` 10
     guard (o == (s + m + c3) `mod` 10)
     guard (m == (s + m + c3) `quot` 10)
     return (s,e,n,d,m,o,r,e,m,o,n,e,y)

-- | /showSoln (s,e,n,d,m,o,r,e,m,o,n,e,y)/ formats a candidate solution to
-- the SENDMOREMONEY problem nicely as a String.

showSoln (s1,e1,n1,d1,m1,o1,r1,e2,m2,o2,n2,e3,y1) =
    "\n"
      ++ topStr
      ++ "\n"
      ++ midStr
      ++ "\n"
      ++ divider
      ++ "\n"
      ++ botStr
      ++ "\n"
      ++ divider
      ++ "\n"
    where
      top = [s1,e1,n1,d1]
      mid = [m1,o1,r1,e2]
      bot = [m2,o2,n2,e3,y1]
      topStr = "  " ++ unwords (map show top)
      midStr = "+ " ++ unwords (map show mid)
      botStr = unwords (map show bot)
      divider = replicate 9 '-'


