module SBVTest4 where

import Data.SBV
import SBVExts2
import Mod

{-|
>>> allSat p0
Solution #1:
  s0 = 0 :: Word4
Solution #2:
  s0 = 1 :: Word4
Solution #3:
  s0 = 2 :: Word4
Found 3 different solutions.

>>> allSat p0 >>= \r -> return $ (extractModels r :: [[Mod]])
[[0],[1],[2]]
-}
p0 :: SMod -> SBool
p0 x = x .== x

{-|
>>> allSat p1
Solution #1:
  s0 = 1 :: Word4
Solution #2:
  s0 = 2 :: Word4
Found 2 different solutions.

>>> allSat p1 >>= \r -> return $ (extractModels r :: [[Mod]])
[[1],[2]]
-}
p1 :: SMod -> SBool
p1 x = x * x .== 1

{-|
>>> allSat p2
Solution #1:
  s0 = 0 :: Word4
  s3 = 1 :: Word4
Solution #2:
  s0 = 1 :: Word4
  s3 = 0 :: Word4
Solution #3:
  s0 = 2 :: Word4
  s3 = 2 :: Word4
Found 3 different solutions.

>>> allSat p2 >>= \r -> return $ (extractModels r :: [[Mod]])
[[0,1],[1,0],[2,2]]
-}
p2 :: SMod -> SMod -> SBool
p2 x y = x + y .== 1
