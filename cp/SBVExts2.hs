{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies #-}

module SBVExts2 where

import Data.Functor.Identity
import GHC.Exts (Constraint)

import Data.SBV

class Eq2 repl where
  infix 4 @==, @/=
  (@==), (@/=) :: Eq a => repl a -> repl a -> repl Bool

instance Eq2 Identity where
  x @== y = (==) <$> x <*> y
  x @/= y = (/=) <$> x <*> y

instance Eq2 SBV where
  (@==) = (.==)
  (@/=) = (./=)

class Eq3 f where
  infixl 4 `eq3`
  eq3 :: (Eq2 repl, Eq a) => f (repl a) -> f (repl a) -> repl Bool

instance Eq3 Identity where
  Identity x `eq3` Identity y = x @== y

class Eq2 repl => Ord2 repl where
  type Ord2Elem repl a :: Constraint
  infix 4 @<, @<=, @>, @>=
  (@<), (@<=), (@>), (@>=) :: Ord2Elem repl a => repl a -> repl a -> repl Bool
  min', max' :: Ord2Elem repl a => repl a -> repl a -> repl a

instance Ord2 Identity where
  type Ord2Elem Identity a = Ord a
  x @<  y = (<)  <$> x <*> y
  x @<= y = (<=) <$> x <*> y
  x @>  y = (>)  <$> x <*> y
  x @>= y = (>=) <$> x <*> y
  min' = min
  max' = max

instance Ord2 SBV where
  type Ord2Elem SBV a = SymWord a
  (@<)  = (.<)
  (@<=) = (.<=)
  (@>)  = (.>)
  (@>=) = (.>=)
  min' = smin
  max' = smax

deriving instance Num a => Num (Identity a)
deriving instance SDivisible a => SDivisible (Identity a)
