{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-
Modular arithmetic based on Data.SBV.Examples.Misc.Word4
-}

module Mod
       ( Mod
       , SMod
       ) where

import Data.Generics (Data, Typeable)
import Data.Proxy
import GHC.Enum      (boundedEnumFrom, boundedEnumFromThen, predError,
                      succError)
import GHC.TypeLits
import System.Random (Random (..))

import Data.SBV
import Data.SBV.Dynamic
import Data.SBV.Internals

-- | Mod as a newtype. Invariant: @Mod x@ should satisfy @x < mm@.
newtype Mod (m :: Nat) = Mod { unMod :: Word8 }
  deriving (Eq, Ord, Data, Typeable)

mm :: forall m. KnownNat m => Mod m
mm = Mod . fromInteger . natVal $ (Proxy :: Proxy m)

-- | Smart constructor; simplifies conversion from Word8
modm :: forall m. KnownNat m => Word8 -> Mod m
modm x = Mod (x `sMod` unMod (mm :: Mod m))

-- | Show instance
instance Show (Mod m) where
  show (Mod x) = show x

-- | Read instance. We read as an 8-bit word, and coerce
instance KnownNat m => Read (Mod m) where
  readsPrec p s = [ (modm x, s') | (x, s') <- readsPrec p s ]

-- | Bounded instance; from 0 to mm-1
instance KnownNat m => Bounded (Mod m) where
  minBound = Mod 0
  maxBound = mm - 1

-- | Enum instance, trivial definitions.
instance KnownNat m => Enum (Mod m) where
  succ m@(Mod x) = if m < mm-1 then Mod (succ x) else succError "Mod"
  pred m@(Mod x) = if m > 0    then Mod (pred x) else predError "Mod"
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger . unMod
  -- Comprehensions
  enumFrom                                     = boundedEnumFrom
  enumFromThen                                 = boundedEnumFromThen
  enumFromTo     (Mod x) (Mod y)           = map Mod (enumFromTo x y)
  enumFromThenTo (Mod x) (Mod y) (Mod z) = map Mod (enumFromThenTo x y z)

-- | Num instance, merely lifts underlying 8-bit operation and casts back
instance KnownNat m => Num (Mod m) where
  Mod x + Mod y = modm (x + y)
  Mod x * Mod y = modm (x * y)
  Mod x - Mod y = modm (x - y)
  negate (Mod x)  = modm (negate x)
  abs (Mod x)     = Mod x
  signum (Mod x)  = Mod (if x == 0 then 0 else 1)
  fromInteger n     = modm (fromInteger n)

-- | Real instance simply uses the Word8 instance
instance KnownNat m => Real (Mod m) where
  toRational (Mod x) = toRational x

-- | Integral instance, again using Word8 instance and casting. NB. we do
-- not need to use the smart constructor here as neither the quotient nor
-- the remainder can overflow a Mod.
instance KnownNat m => Integral (Mod m) where
  quotRem (Mod x) (Mod y) = (Mod q, Mod r)
    where (q, r) = quotRem x y
  toInteger (Mod x) = toInteger x

-- | Random instance, used in quick-check
instance KnownNat m => Random (Mod m) where
  randomR (Mod lo, Mod hi) gen = (Mod x, gen')
    where (x, gen') = randomR (lo, hi) gen
  random gen = (Mod x, gen')
    where (x, gen') = randomR (0, unMod (mm :: Mod m) - 1) gen

-- | SMod type synonym
type SMod m = SBV (Mod m)

svMM :: KnownNat m => SMod m
svMM = literal mm

-- | Size enough for result of multiplication
modBits :: Int
modBits = 8

modKind :: Kind
modKind = KBounded False modBits

-- | SymWord instance, allowing this type to be used in proofs/sat etc.
instance KnownNat m => SymWord (Mod m) where
  mkSymWord q n = do
    x <- genMkSymVar modKind q n
    constrain $ x .<= literal (modm (unMod (mm :: Mod m) - 1))
    return x
  literal    = genLiteral modKind
  fromCW     = genFromCW

-- | HasKind instance; simply returning the underlying kind for the type
instance HasKind (Mod m) where
  kindOf _ = modKind

-- | SatModel instance, merely uses the generic parsing method.
instance KnownNat m => SatModel (Mod m) where
  parseCWs = genParse modKind

-- | SDvisible instance, using 0-extension
instance KnownNat m => SDivisible (Mod m) where
  sQuotRem x 0 = (0, x)
  sQuotRem x y = x `quotRem` y
  sDivMod  x 0 = (0, x)
  sDivMod  x y = x `divMod` y

instance {-# OVERLAPPING #-} KnownNat m => Num (SMod m) where
  fromInteger = literal . fromInteger
  SBV x + SBV y = SBV (svPlus x y) `sMod` svMM
  SBV x * SBV y = SBV (svTimes x y) `sMod` svMM
  abs = id  -- donothing since the value is always non-negative
  signum x = ite (x .== 0) (literal 0) (literal 1)
  negate _ = error "Num.SMod.negate: not supported"

-- | SDvisible instance, using default methods
instance KnownNat m => SDivisible (SMod m) where
  sQuotRem = liftQRem
  sDivMod  = liftDMod
