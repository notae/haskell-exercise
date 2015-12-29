{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module SBVTest2 where

import Data.Generics
import Data.SBV

import SBVTest (Color (..), SColor)

class (SatModel a, SatVar (Var a)) => SatSpace a where
  type Var a

class SatVar a where
  varExists :: Symbolic a

allSat' :: SatSpace a => (Var a -> Predicate) -> IO [a]
allSat' p = (allSat $ varExists >>= p) >>= return . extractModels

spc :: SatSpace a =>
       (Predicate -> IO AllSatResult) -> (Var a -> Predicate) -> IO [a]
spc op p = (op $ varExists >>= p) >>= return . extractModels

data V_ i c = V { vi :: i, vc :: c }
            deriving (Show, Eq)
type V = V_ Integer Color

instance SatModel V where
  parseCWs is = do (i, cs) <- parseCWs is
                   (c, xs) <- parseCWs cs
                   return (V i c, xs)

instance SatVar (V_ SInteger SColor) where
  varExists = do
    (i :: SInteger) <- exists_
    (c :: SColor) <- exists_
    return $ V i c

instance SatSpace (V_ Integer Color) where
  type Var (V_ Integer Color) = V_ SInteger SColor

{-|
>>> test1
[V {vi = -1, vc = Red},V {vi = 0, vc = Green},V {vi = 1, vc = Blue},V {vi = -1, vc = Yellow},V {vi = 1, vc = Red},V {vi = -1, vc = Green},V {vi = 0, vc = Red},V {vi = 1, vc = Yellow},V {vi = 1, vc = Green},V {vi = 0, vc = Yellow},V {vi = 0, vc = Blue},V {vi = -1, vc = Blue}]
-}
test1 :: IO [V]
test1 = allSat' $ \(V i c) -> return $ abs i .<= 1

{-|
>>> (==) <$> test1 <*> test2
True
-}
test2 :: IO [V]
test2 = spc allSat $ \(V i c) -> return $ abs i .<= 1
