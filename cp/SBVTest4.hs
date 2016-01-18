{-# LANGUAGE DataKinds #-}

module SBVTest4 where

import Data.Functor.Identity
import Data.SBV
import Mod
import SBVExts2

{- $setup
>>> :set -XDataKinds
-}

p0 :: (Eq a, Eq2 repl) => repl a -> repl Bool
p0 x = x @== x

{-|
>>> allSat p0S
Solution #1:
  s0 = 0 :: Word8
Solution #2:
  s0 = 1 :: Word8
Solution #3:
  s0 = 2 :: Word8
Found 3 different solutions.

>>> allSat p0S >>= \r -> return $ (extractModels r :: [[Mod 3]])
[[0],[1],[2]]
-}
p0S :: SMod 3 -> SBool
p0S = p0

{-|
>>> runIdentity (p0I 2)
True
-}
p0I :: Identity (Mod 3) -> Identity Bool
p0I = p0

p1 :: (Eq a, Num (repl a), Eq2 repl) => repl a -> repl Bool
p1 x = x * x @== 1

{-|
>>> allSat p1S
Solution #1:
  s0 = 1 :: Word8
Solution #2:
  s0 = 2 :: Word8
Found 2 different solutions.

>>> allSat p1S >>= \r -> return $ (extractModels r :: [[Mod 3]])
[[1],[2]]
-}
p1S :: SMod 3 -> SBool
p1S = p1

{-|
>>> runIdentity $ p1I 0
False
>>> runIdentity $ p1I 1
True
>>> runIdentity $ p1I 2
True
-}
p1I :: Identity (Mod 3) -> Identity Bool
p1I = p1

p2 :: (Eq a, Num (repl a), Eq2 repl) => repl a -> repl a -> repl Bool
p2 x y = x + y @== 1

{-|
>>> allSat p2S
Solution #1:
  s0 = 0 :: Word8
  s3 = 1 :: Word8
Solution #2:
  s0 = 1 :: Word8
  s3 = 0 :: Word8
Solution #3:
  s0 = 2 :: Word8
  s3 = 2 :: Word8
Found 3 different solutions.

>>> allSat p2S >>= \r -> return $ (extractModels r :: [[Mod 3]])
[[0,1],[1,0],[2,2]]
-}
p2S :: SMod 3 -> SMod 3 -> SBool
p2S = p2

{-|
>>> runIdentity $ p2I 2 2
True
>>> runIdentity $ p2I 2 1
False
-}
p2I :: Identity (Mod 3) -> Identity (Mod 3) -> Identity Bool
p2I = p2
