module Modular where

import Data.SBV

{-
Define modular arithmetic field as newtype
-}
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
