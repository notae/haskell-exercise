-- Example of number-parameterized types

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module NumberParamterizedTypes
       (
         RNat
       , (/)
       ) where

import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)

-- Natural number restricted with an upper bound

newtype i `RNat` (n :: Nat) = RNat { unRNat :: i } deriving (Eq, Ord)

type (/) = RNat

toRNat :: forall i n . (Integral i, KnownNat n) => i -> i `RNat` n
toRNat i = if (unRNat (minBound :: i `RNat` n)) <= i &&
              i <= (unRNat (maxBound :: i `RNat` n))
           then RNat i
           else error "NumberParamterizedTypes.toRNat: bad argument"

instance Show i => Show (i `RNat` n) where
  show = show . unRNat

{-|
Num instance is only for literals.
>>> :t 1 :: Int `RNat` 3
1 :: Int `RNat` 3 :: RNat Int 3
>>> 1 :: Int `RNat` 3
1
-}
instance (Integral i, KnownNat n) => Num (i `RNat` n) where
  fromInteger = toRNat . fromInteger

{-|
>>> :t [0..2] :: [Int/5]
[0..2] :: [Int/5] :: [Int / 5]
>>> [0..2] :: [Int/5]
[0,1,2]
-}
instance (Integral i, KnownNat n) => Enum (i `RNat` n) where
  succ (RNat i) = toRNat (succ i)
  fromEnum = fromInteger . toInteger . unRNat
  toEnum = toRNat .fromInteger . toInteger

{-|
>>> minBound :: Int/5
0
>>> maxBound :: Int/5
4
-}
instance (Integral i, KnownNat n) => Bounded (i `RNat` n) where
  minBound = RNat 0
  maxBound = RNat (fromInteger (natVal (Proxy :: Proxy n)) - 1)
