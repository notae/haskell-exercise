{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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

instance (SymWord a, SymWord b) => SatVar (SBV a, SBV b) where
  varExists = do
    (a :: SBV a) <- exists_
    (b :: SBV b) <- exists_
    return (a, b)

instance (SymWord a, SymWord b, SatModel (a, b)) => SatSpace (a, b) where
  type Var (a, b) = (SBV a, SBV b)

data V_ i c = V { vi :: i, vc :: c }
            deriving (Show)
type V = V_ Integer Color

instance SatModel (V_ Integer Color) where
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

test1 :: IO [(Color, Color)]
test1 = allSat' $ \(x, y) -> return $ x .== y

test2 :: IO [V]
test2 = allSat' $ \(V i c) -> return $ abs i .<= 1
