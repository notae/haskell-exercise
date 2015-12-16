{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module TypedCP2 where

import Control.Monad
import Control.Monad.Operational
import Data.Coerce
import Data.Proxy
import GHC.TypeLits

type IsFDValue a = (Ord a, Enum a)

-- An example type of FDValue
data Color =
  Red | Green | Blue
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype FDValue a =
  V Int
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype RangedInt (l :: Nat) (u :: Nat) =
  RInt Int
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance (KnownNat l, KnownNat u) => Num (RangedInt l u) where
  fromInteger i = if (natVal (Proxy :: Proxy l)) <= i &&
                     i <= (natVal (Proxy :: Proxy u))
                  then RInt (fromInteger i)
                  else error $ "fromInteger: invalid value: " ++ show i

--  (RInt x) + (RInt y) = maximum []

type Deg = RangedInt 1 7

-- OK
d3 :: Deg
d3 = 3

-- Wish to detect the invalid value at compile time
d8 :: Deg
d8 = 8

-- Interval
type family IAdd (m :: (Nat, Nat)) (n :: (Nat, Nat)) :: (Nat, Nat)
type instance IAdd '(l, u) '(l', u') = '(l+l', u+u')

type family IAdd' (l :: Nat) (u :: Nat) (l' :: Nat) (u' :: Nat) :: (Nat, Nat)
type instance IAdd' l u l' u' = '(l+l', u+u')

data I (r :: (Nat, Nat)) = I deriving (Show)

{-|
>>> :t i
i :: I '(3, 8)
-}
i :: I ('(1, 3) `IAdd` '(2, 5))
i = I

{-
Domain:
  example in MiniZinc
    var 1..3: a;
    var 2..5: b;
    constraint a = b   % domains of a and b is reduced to 2..3
-}

