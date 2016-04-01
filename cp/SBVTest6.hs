{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module SBVTest6 where

import Control.Applicative
import Data.Functor.Identity
import Data.SBV
import Text.Show.Functions   ()


class BoolExpr repl where
  bool :: Bool -> repl Bool
  (~&&) :: repl Bool -> repl Bool -> repl Bool

instance BoolExpr Identity where
  bool = Identity
  (~&&) = liftA2 (&&)

instance BoolExpr SBV where
  bool = literal
  (~&&) = (&&&)

newtype S a = S { unS :: String } deriving Show

instance BoolExpr S where
  bool x = S $ "bool " ++ show x
  S x ~&& S y = S $ x ++ " ~&& " ++ y


class ComplexExpr repl where
  comp :: Int8 -> Int8 -> repl Int8
  cadd :: repl Int8 -> repl Int8 -> repl Int8

newtype Complex a = Complex (a, a) deriving Show

instance ComplexExpr Complex where
  comp x y = Complex (x, y)
  cadd (Complex (x, y)) (Complex (x', y')) = Complex (x + x', y + y')

newtype SComplex a = SComplex (SBV a, SBV a) deriving Show

instance ComplexExpr SComplex where
  comp x y = SComplex (literal x, literal y)
  cadd (SComplex (x, y)) (SComplex (x', y')) = SComplex (x + x', y + y')

instance ComplexExpr S where
  comp x y = S $ show (x, y)
  cadd (S x) (S y) = S $ "(" ++ x ++ " `cadd` " ++ y ++ ")"


class ComplexBoolExpr repl where
  type C repl :: * -> *
  ceq :: C repl Int8 -> C repl Int8 -> repl Bool

instance ComplexBoolExpr Identity where
  type C Identity = Complex
  ceq (Complex (x, y)) (Complex (x', y')) = bool $ x == x' && y == y'

instance ComplexBoolExpr SBV where
  type C SBV = SComplex
  ceq (SComplex (x, y)) (SComplex (x', y')) = x .== x' &&& y .== y'

instance ComplexBoolExpr S where
  type C S = S
  ceq (S x) (S y) = S $ "(" ++ x ++ " `ceq` " ++ y ++ ")"


c1 :: ComplexExpr repl => repl Int8
c1 = comp 1 2
c2 :: ComplexExpr repl => repl Int8
c2 = comp 2 3
c3 :: ComplexExpr repl => repl Int8
c3 = comp 3 5

{-|
>>> putStrLn $ unS $ tceq ~&& bool False
(((1,2) `cadd` (2,3)) `ceq` (3,5)) ~&& bool False
>>> runIdentity $ tceq ~&& bool False
False
>>> tceq ~&& bool False :: SBool
False
-}
tceq :: (ComplexExpr (C repl), ComplexBoolExpr repl) => repl Bool
tceq = (c1 `cadd` c2) `ceq` c3
