{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module SBVExts
       ( SatSpace, Val
       , SatVar (..)
       , allSat', allSatWith'
       ) where

import Data.Generics

import Data.SBV

{-
class (SatModel a, SatVar (Var a)) => SatSpace a where
  type Var a

class SatVar a where
  varExists :: Symbolic a

instance SymWord a => SatVar (SBV a) where
  varExists = exists_

instance (SatModel a, SymWord a) => SatSpace a where
  type Var a = SBV a

allSat' :: SatSpace a => (a -> Predicate) -> IO [a]
allSat' p = (allSat $ varExists >>= p) >>= return . extractModels

spc :: SatSpace a =>
       (Predicate -> IO AllSatResult) -> (a -> Predicate) -> IO [a]
spc op p = (op $ varExists >>= p) >>= return . extractModels

data V_ i c = V { vi :: i, vc :: c } deriving (Show, Eq)
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

instance SatSpace V where
  type Var V = V_ SInteger SColor
-}

{-
class (SatModel a, SatVar v) => SatSpace a v where

class SatVar v where
  varExists :: Symbolic v

instance SymWord a => SatVar (SBV a) where
  varExists = exists_

instance (SatModel a, SymWord a) => SatSpace a (SBV a) where

allSat' :: SatSpace a v => (v -> Predicate) -> IO [a]
allSat' p = (allSat $ varExists >>= p) >>= return . extractModels

spc :: SatSpace a v =>
       (Predicate -> IO AllSatResult) -> (v -> Predicate) -> IO [a]
spc op p = (op $ varExists >>= p) >>= return . extractModels

data V_ i c = V { vi :: i, vc :: c } deriving (Show, Eq)
type V = V_ Integer Color
type SV = V_ SInteger SColor

instance (SatModel i, SatModel c) => SatModel (V_ i c) where
  parseCWs is = do (i, cs) <- parseCWs is
                   (c, xs) <- parseCWs cs
                   return (V i c, xs)

instance (SymWord i, SymWord c) => SatVar (V_ (SBV i) (SBV c)) where
  varExists = do
    i <- exists_
    c <- exists_
    return $ V i c

instance (SymWord i, SymWord c, SatModel i, SatModel c) =>
         SatSpace (V_ i c) (V_ (SBV i) (SBV c)) where
-}

class (SatModel (Val a), SatVar a) => SatSpace a where
  type Val a

class SatVar a where
  varExists :: Symbolic a

instance SymWord a => SatVar (SBV a) where
  varExists = exists_

instance (SatModel a, SymWord a) => SatSpace (SBV a) where
  type Val (SBV a) = a

allSat' :: SatSpace a => (a -> Predicate) -> IO [Val a]
allSat' p = (allSat $ varExists >>= p) >>= return . extractModels

allSatWith' :: SatSpace a => SMTConfig -> (a -> Predicate) -> IO [Val a]
allSatWith' config p =
  (allSatWith config $ varExists >>= p) >>= return . extractModels
