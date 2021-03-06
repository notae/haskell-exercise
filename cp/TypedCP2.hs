{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module TypedCP2 where

import Control.Monad
import Control.Monad.Operational
import Data.Coerce
import Data.Proxy
import GHC.TypeLits

import RangedInt

--
-- RangedInt examples
--

-- ric1 :: RangedInt 1 1
ric1 = $(ric 1)

ri1 :: RangedInt 1 1
ri1 = 2

f1 :: RangedInt 3 5 -> RangedInt 5 7
f1 x = x @+ $(ric 2)

f2 :: RangedInt l u -> RangedInt (l + 2) (u + 2)
f2 x = x @+ $(ric 2)

f3 :: RangedInt 0 31 -> RangedInt 0 15 -> RangedInt 0 (31+15)
f3 x y = x @+ y

-- ex. ranged to [1, 7]
type Deg = RangedInt 1 7

-- OK
d3 :: Deg
d3 = 3

-- Wish to detect the invalid value at compile time
d8 :: Deg
d8 = 8


--
-- Coerce into Int using Enum class
--

type IsFDValue a = (Ord a, Enum a)

data Color =
  Red | Green | Blue
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype FDValue a =
  V Int
  deriving (Eq, Ord, Enum, Bounded)

mkV :: Enum a => a -> FDValue a
mkV a = V (fromEnum a)

unV :: Enum a => FDValue a -> a
unV (V i) = toEnum i

instance (Show a, Enum a) => Show (FDValue a) where
  show (V i) = show (toEnum i :: a)

pred1 :: Color -> Color -> Bool
pred1 x     y     | x == y = True
pred1 Red   Blue  = True
pred1 Blue  Red   = True
pred1 Green Blue  = True
pred1 Blue  Green = True
pred1 _     _     = False

evalPred :: Enum a => (a -> a -> Bool) -> FDValue a -> FDValue a -> Bool
evalPred p x y = p (unV x) (unV y)

--
-- Validate at compile time
--

type family IAdd (m :: (Nat, Nat)) (n :: (Nat, Nat)) :: (Nat, Nat)
type instance IAdd '(l, u) '(l', u') = '(l+l', u+u')

type family IAdd' (l :: Nat) (u :: Nat) (l' :: Nat) (u' :: Nat) :: (Nat, Nat)
type instance IAdd' l u l' u' = '(l+l', u+u')

{-|
>>> :t i
i :: I '(3, 8)
-}
i :: I ('(1, 3) `IAdd` '(2, 5))
i = I

data I (r :: (Nat, Nat)) = I deriving (Show)

iadd :: I x -> I y -> I (x `IAdd` y)
x `iadd` y = I

ix :: I '(1, 3)
ix = I
iy :: I '(2, 5)
iy = I
iz :: I '(3, 8)
iz = ix `iadd` iy

data P (p :: Nat) = P deriving (Show)
type P' (p :: Nat) = Proxy p

iread :: (l <= p, p <= u, KnownNat p) => P p -> I '(l, u) -> Int
iread p i = fromInteger $ natVal p

p2 :: P 2
p2 = P
p4 :: P 4
p4 = P

-- Check given point in the interval at compile time
r2 = iread p2 ix  -- valid
-- r4 = iread p4 ix  -- type error

{-
Domain:
  example in MiniZinc
    var 1..3: a;
    var 2..5: b;
    constraint a = b   % domains of a and b is reduced to 2..3
-}

