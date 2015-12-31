{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable #-}

module SBVTest2 where

import Control.Arrow (first)
import Control.Monad (forM_, replicateM)
import Data.Generics
import GHC.TypeLits

import Data.SBV

import SBVExts
import SBVTest (Color (..), SColor)

{-
an example of user-defined data type
-}
data V_ i c = V { vi :: i, vc :: c } deriving (Show, Eq, Ord)
type V = V_ Integer Color
type SV = V_ SInteger SColor

instance SatModel V where
  parseCWs is = do (i, cs) <- parseCWs is
                   (c, xs) <- parseCWs cs
                   return (V i c, xs)

instance SatVar SV where
  varExists = exists_ >>= \i -> exists_ >>= \c -> return $ V i c

instance SatSpace SV where
  type Val SV = V

{-|
>>> test1
[V {vi = -1, vc = Red},V {vi = 0, vc = Green},V {vi = 1, vc = Blue},V {vi = -1, vc = Yellow},V {vi = 1, vc = Red},V {vi = -1, vc = Green},V {vi = 0, vc = Red},V {vi = 1, vc = Yellow},V {vi = 1, vc = Green},V {vi = 0, vc = Yellow},V {vi = 0, vc = Blue},V {vi = -1, vc = Blue}]
-}
test1 :: IO [V]
test1 = allSat' $ \((V i c) :: SV) -> return $ abs i .<= 1

{-|
>>> (==) <$> test1 <*> test2
True
-}
test2 :: IO [V]
test2 = spc allSat $ \((V i c) :: SV) -> return $ abs i .<= 1

{-|
>>> test3
[-1,0,1]
-}
test3 :: IO [Integer]
test3 = spc allSat $ \(i :: SInteger) -> return $ abs i .<= 1

{-
an example of user-defined container type
-}
newtype SizedList (l :: Nat) a =
  SizedList [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

mkSList :: forall l a. KnownNat l => [a] -> SizedList l a
mkSList xs =
  if natVal (Proxy :: Proxy l) == l
  then SizedList xs
  else error $ "mkSList: invalid size: " ++ show l
  where l = toInteger (length xs)

lengthSList :: forall l a. KnownNat l => SizedList l a -> Integer
lengthSList _ = natVal (Proxy :: Proxy l)

instance (SatModel (Val (SizedList l v)), SatVar (SizedList l v),
          SatSpace v, KnownNat l) =>
         SatSpace (SizedList l v) where
  type Val (SizedList l v) = SizedList l (Val v)

instance (SatModel a, KnownNat l) => SatModel (SizedList l a) where
  parseCWs xs = (first mkSList) <$> parseCWs0 l xs
    where
      l = natVal (Proxy :: Proxy l)
      parseCWs0 :: Integer -> [CW] -> Maybe ([a], [CW])
      parseCWs0 _ [] = Just ([], [])
      parseCWs0 0 _  = Just ([], [])
      parseCWs0 l xs =
        case parseCWs xs of
         Just (a, ys) -> case parseCWs0 (l - 1) ys of
                          Just (as, zs) -> Just (a:as, zs)
                          Nothing       -> Just ([], ys)
         Nothing     -> Just ([], xs)

instance (SatVar v, KnownNat l) => SatVar (SizedList l v) where
  varExists = do
    let l = fromInteger $ natVal (Proxy :: Proxy l)
    mkSList <$> replicateM l varExists

{-|
>>> extractSLists
[SizedList [0,0,0],SizedList [0,0,1],SizedList [1,0,0],SizedList [1,0,1],SizedList [0,1,0],SizedList [0,1,1],SizedList [1,1,0],SizedList [1,1,1]]
>>> length <$> extractSLists
8
-}
extractSLists :: IO [SizedList 3 Integer]
extractSLists = spc allSat $ \(SizedList xs :: SizedList 3 SInteger) -> do
  forM_ xs $ \x -> constrain $ 0 .<= x &&& x .<= 1
  return $ (true :: SBool)

instance (SatModel (Val (a, b)), SatVar (a, b)) => SatSpace (a, b) where
  type Val (a, b) = (Val a, Val b)

instance (SatVar a, SatVar b) => SatVar (a, b) where
  varExists = varExists >>= \a -> varExists >>= \b -> return (a, b)

{-|
>>> length <$> extractSLists2
144
-}
extractSLists2 :: IO [SizedList 2 V]
extractSLists2 = spc allSat $ \(SizedList xs :: SizedList 2 SV) -> do
  forM_ xs $ \(V i c) -> constrain $ abs i .<= 1
  return $ (true :: SBool)

{-
TBD: variable length list
-}
