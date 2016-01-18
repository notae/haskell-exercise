{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-
Modular arithmetic based on Data.SBV.Examples.Misc.Word4
-}

module Mod where

import GHC.Enum (boundedEnumFrom, boundedEnumFromThen, predError, succError,
                 toEnumError)

import Data.Bits
import Data.Generics (Data, Typeable)
import System.Random (Random (..))

import Data.SBV
import Data.SBV.Dynamic
import Data.SBV.Internals

mm :: Num a => a
mm = 3

-- | Mod as a newtype. Invariant: @Mod x@ should satisfy @x < mm@.
newtype Mod = Mod Word8
  deriving (Eq, Ord, Data, Typeable)

-- | Smart constructor; simplifies conversion from Word8
modm :: Word8 -> Mod
modm x = Mod (x `sMod` mm)

-- | Show instance
instance Show Mod where
  show (Mod x) = show x

-- | Read instance. We read as an 8-bit word, and coerce
instance Read Mod where
  readsPrec p s = [ (modm x, s') | (x, s') <- readsPrec p s ]

-- | Bounded instance; from 0 to mm-1
instance Bounded Mod where
  minBound = Mod 0
  maxBound = Mod (mm-1)

-- | Enum instance, trivial definitions.
instance Enum Mod where
  succ (Mod x) = if x < mm-1 then Mod (succ x) else succError "Mod"
  pred (Mod x) = if x > 0    then Mod (pred x) else predError "Mod"
  toEnum i | 0 <= i && i <= mm-1 = Mod (toEnum i)
           | otherwise              = toEnumError "Mod" i (Mod 0, Mod (mm-1))
  fromEnum (Mod x) = fromEnum x
  -- Comprehensions
  enumFrom                                     = boundedEnumFrom
  enumFromThen                                 = boundedEnumFromThen
  enumFromTo     (Mod x) (Mod y)           = map Mod (enumFromTo x y)
  enumFromThenTo (Mod x) (Mod y) (Mod z) = map Mod (enumFromThenTo x y z)

-- | Num instance, merely lifts underlying 8-bit operation and casts back
instance Num Mod where
  Mod x + Mod y = modm (x + y)
  Mod x * Mod y = modm (x * y)
  Mod x - Mod y = modm (x - y)
  negate (Mod x)  = modm (negate x)
  abs (Mod x)     = Mod x
  signum (Mod x)  = Mod (if x == 0 then 0 else 1)
  fromInteger n     = modm (fromInteger n)

-- | Real instance simply uses the Word8 instance
instance Real Mod where
  toRational (Mod x) = toRational x

-- | Integral instance, again using Word8 instance and casting. NB. we do
-- not need to use the smart constructor here as neither the quotient nor
-- the remainder can overflow a Mod.
instance Integral Mod where
  quotRem (Mod x) (Mod y) = (Mod q, Mod r)
    where (q, r) = quotRem x y
  toInteger (Mod x) = toInteger x

-- | Random instance, used in quick-check
instance Random Mod where
  randomR (Mod lo, Mod hi) gen = (Mod x, gen')
    where (x, gen') = randomR (lo, hi) gen
  random gen = (Mod x, gen')
    where (x, gen') = randomR (0, mm-1) gen

-- | SMod type synonym
type SMod = SBV Mod

svMM :: SMod
svMM = literal (Mod mm)

-- | Size enough for result of multiplication
modBits :: Int
modBits = ceiling (logBase 2 mm * 2 :: Float)

modKind :: Kind
modKind = KBounded False modBits

-- | SymWord instance, allowing this type to be used in proofs/sat etc.
instance SymWord Mod where
  mkSymWord q n = do
    x <- genMkSymVar modKind q n
    constrain $ x .<= literal (modm (mm-1))
    return x
  literal    = genLiteral modKind
  fromCW     = genFromCW

-- | HasKind instance; simply returning the underlying kind for the type
instance HasKind Mod where
  kindOf _ = modKind

-- | SatModel instance, merely uses the generic parsing method.
instance SatModel Mod where
  parseCWs = genParse modKind

-- | SDvisible instance, using 0-extension
instance SDivisible Mod where
  sQuotRem x 0 = (0, x)
  sQuotRem x y = x `quotRem` y
  sDivMod  x 0 = (0, x)
  sDivMod  x y = x `divMod` y

instance {-# OVERLAPPING #-} Num SMod where
  fromInteger = literal . fromInteger
  SBV x + SBV y = SBV (svPlus x y) `sMod` svMM
  SBV x * SBV y = SBV (svTimes x y) `sMod` svMM
  abs = id  -- donothing since the value is always non-negative
  signum x = ite (x .== 0) (literal 0) (literal 1)
  negate _ = error "Num.SMod.negate: not supported"

-- | SDvisible instance, using default methods
instance SDivisible SMod where
  sQuotRem = liftQRem
  sDivMod  = liftDMod
