{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module RangedInt
       ( RangedInt
       , JustInt
       , ric
       , (@+)
       ) where

import Data.Proxy
import GHC.TypeLits
import Language.Haskell.TH


--
-- Validate values in constructor at runtime
--

newtype RangedInt (l :: Nat) (u :: Nat) =
  RInt Int
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance (KnownNat l, KnownNat u) => Num (RangedInt l u) where
  fromInteger i = if (natVal (Proxy :: Proxy l)) <= i &&
                     i <= (natVal (Proxy :: Proxy u))
                  then RInt (fromInteger i)
                  else error $ "fromInteger: invalid value: " ++ show i

type JustInt i = RangedInt i i

ric :: Integer -> Q Exp
ric i = sigE (litE (IntegerL i))
        (appT (conT (mkName "JustInt")) (litT (numTyLit i)))

infixl 6 @+
(@+) :: RangedInt xl xu -> RangedInt yl yu -> RangedInt (xl + yl) (xu + yu)
(RInt x) @+ (RInt y) = RInt (x + y)
