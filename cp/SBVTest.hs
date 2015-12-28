module SBVTest where

import Data.SBV

-- | Pythagorean triple
pyt :: SInteger -> (SInteger, SInteger, SInteger) -> SBool
pyt n (x, y, z) =
  x * x + y * y .== z * z &&&
  1 .<= x &&& x .<= n &&&
  1 .<= y &&& y .<= n &&&
  1 .<= z &&& z .<= n &&&
  x .<= y

{-|
>>> allSatPyt 15
Solution #1:
  s0 = 9 :: Integer
  s1 = 12 :: Integer
  s2 = 15 :: Integer
Solution #2:
  s0 = 5 :: Integer
  s1 = 12 :: Integer
  s2 = 13 :: Integer
Solution #3:
  s0 = 6 :: Integer
  s1 = 8 :: Integer
  s2 = 10 :: Integer
Solution #4:
  s0 = 3 :: Integer
  s1 = 4 :: Integer
  s2 = 5 :: Integer
Found 4 different solutions.
-}
allSatPyt = allSat . pyt
