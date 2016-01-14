module SBVTest3 where

import Data.Functor.Identity

import Data.SBV

import SBVExts2

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


p0 :: SWord8 -> Symbolic SBool
p0 x = do
  constrain $ x `inRange` (0, 2)
  return $ x * x .== 1

t0 = allSat p0


newtype M a = M { unM :: a }
            deriving (Show, Read, Eq, Ord)

-- instance Applicative M where
--   pure = M
--   M f <*> M a = mkM $ f a

type SM a = SBV (M a)

-- instance Eq2 M where
--   M x @== M y = x @== y

-- TBD: store into the type
mm :: Num a => a
mm = 3

mkM :: (Num a, SDivisible a) => a -> M a
mkM a = M $ a `sMod` mm

instance (Num a, SDivisible a) => Num (M a) where
  fromInteger = mkM . fromInteger
  M x + M y    = mkM $ x + y
  M x * M y    = mkM $ x * y
  abs    (M x) = mkM $ abs x
  signum (M x) = mkM $ signum x
  negate (M x) = mkM $ negate x

-- liftOpM :: (a -> a -> b) -> M

p1 :: SWord8 -> Symbolic SBool
p1 x = do
  constrain $ x `inRange` (0, mm - 1)
--   return $ unM $ (@==) <$> mkM x * mkM x <*> mkM 1
  return $ unM (mkM x * mkM x) @== 1

t1 = allSat p1
