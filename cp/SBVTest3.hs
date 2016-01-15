module SBVTest3 where

import Data.Functor.Identity

import Data.SBV

import SBVExts2
import Modular

{-|
>>> runIdentity $ p2 3
True
>>> allSat (p2 :: SWord8 -> SBool)
Solution #1:
  s0 = 3 :: Word8
This is the only solution.
-}
p2 :: (Num (repl Word8), Eq2 repl) => repl Word8 -> repl Bool
p2 x = 2 + x @== 5

{-|
Apply as normal Haskell function:
>>> runIdentity $ p3 2 3
True
>>> runIdentity $ p3 3 3
False
-}
p3 :: (Eq a, Num (repl a), Eq2 repl) => repl a -> repl a -> repl Bool
p3 x y = x + y @== 5

{-|
Apply as SBV predicate:
>>> allSat p4
Solution #1:
  s0 = 1 :: Word8
  s1 = 4 :: Word8
Solution #2:
  s0 = 2 :: Word8
  s1 = 3 :: Word8
Found 2 different solutions.
-}
p4 :: SWord8 -> SWord8 -> Symbolic SBool
p4 x y = do
  constrain $ x `inRange` (1, 3)
  constrain $ y `inRange` (3, 5)
  constrain $ p3 x y
  return true

{-|
Encode residue field with type Word8
>>> allSat p0
Solution #1:
  s0 = 1 :: Word8
Solution #2:
  s0 = 2 :: Word8
Found 2 different solutions.
-}
p0 :: SWord8 -> Symbolic SBool
p0 x = do
  constrain $ x `inRange` (0, 2)
  return $ (x * x) `sMod` mm .== 1

t0 = allSat p0

liftOpM :: (a -> a -> b) -> M a -> M a -> b
liftOpM f (M x) (M y) = f x y

newM :: Symbolic (M SWord8)
newM = do
  x <- free_
  constrain $ x `inRange` (0, mm - 1)
  return $ mkM x

p1 :: Symbolic SBool
p1 = do
  x <- newM
  return $ p11 x

{-|
Apply as normal Haskell function:
>>> runIdentity $ p11 (mkM 2)
True
-}
p11 :: (Num (repl Word8), SDivisible (repl Word8), Eq2 repl) =>
       M (repl Word8) -> repl Bool
p11 x = liftOpM (@==) (x * x) 1

{-|
Apply as SBV predicate:
>>> t1
Solution #1:
  s0 = 2 :: Word8
Solution #2:
  s0 = 1 :: Word8
Found 2 different solutions.
-}
t1 = allSat p1
