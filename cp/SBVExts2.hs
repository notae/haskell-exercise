{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module SBVExts2 where

import Data.Functor.Identity

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

class Eq2 repl => Ord2 repl where
  infix 4 @<, @<=, @>, @>=
  (@<), (@<=), (@>), (@>=) :: Ord a => repl a -> repl a -> repl Bool
  min', max' :: Ord a => repl a -> repl a -> repl a

instance Ord2 Identity where
  x @<  y = (<)  <$> x <*> y
  x @<= y = (<=) <$> x <*> y
  x @>  y = (>)  <$> x <*> y
  x @>= y = (>=) <$> x <*> y
  min' = min
  max' = max

{-
instance Ord2 SBV where
  (@<)  = (.<)
  (@<=) = (.<=)
  (@>)  = (.>)
  (@>=) = (.>=)
  min' = smin
  max' = smax
-}

deriving instance Num a => Num (Identity a)
deriving instance SDivisible a => SDivisible (Identity a)
