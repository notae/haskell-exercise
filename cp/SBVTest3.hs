{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module SBVTest3 where

import Data.Functor.Identity

import Data.SBV

class Eq2 repl where
  infix 4 @==, @/=
  (@==) :: Eq a => repl a -> repl a -> repl Bool
  (@/=) :: Eq a => repl a -> repl a -> repl Bool

instance Eq2 Identity where
  (@==) = \x y -> (==) <$> x <*> y
  (@/=) = \x y -> (/=) <$> x <*> y

instance Eq2 SBV where
  (@==) = (.==)
  (@/=) = (./=)

deriving instance Num a => Num (Identity a)

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
