module Modular where

import Data.SBV
import SBVExts2

{-
Define modular arithmetic field as newtype
-}
newtype M a = M { unM :: a }
            deriving (Show, Read, Eq, Ord)

instance Eq3 M where
  M x `eq3` M y = x @== y

type SM a = SBV (M a)

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
