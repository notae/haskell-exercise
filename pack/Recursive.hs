{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Recursive where

import Data.Functor.Identity
import Text.Show.Functions   ()

import           Data.Set (Set)
import qualified Data.Set as Set


class P p where
  type L p (f :: * -> *)
  plift :: (forall a. a -> f a) -> p -> L p f
  pliftM :: Monad m => (forall a. a -> m (f a)) -> p -> m (L p f)
  pdown :: (forall a. f a -> a) -> L p f -> p
  pdownM :: Monad m => (forall a. f a -> m a) -> L p f -> m p
  pdown' :: Applicative f => L p f -> f p
  -- Default implementation
  type L p f = f p
  default plift :: (forall a. a -> f a) -> p -> f p
  default pliftM :: Monad m => (forall a. a -> m (f a)) -> p -> m (f p)
  default pdown :: (forall a. f a -> a) -> f p -> p
  default pdownM :: Monad m => (forall a. f a -> m a) -> f p -> m p
  default pdown' :: Applicative f => f p -> f p
  plift f a = f a
  pliftM f a = f a
  pdown f a = f a
  pdownM f a = f a
  pdown' a = a

-- Using default implementation
instance P Bool
instance P Int

instance (P a, P b) => P (a, b) where
  type L (a, b) f = (L a f, L b f)
  plift f (a, b) = (plift f a, plift f b)
  pliftM f (a, b) = (,) <$> pliftM f a <*> pliftM f b
  pdown f (a, b) = (pdown f a, pdown f b)
  pdownM f (a, b) = (,) <$> pdownM f a <*> pdownM f b
  pdown' (a, b) = (,) <$> pdown' a <*> pdown' b

{-|
>>> testPLift
([1],[True])
-}
testPLift :: ([Int], [Bool])
testPLift = plift pure (1::Int, True)

{-|
>>> testPLiftWithCtx
(fromList [1],fromList [True])
-}
testPLiftWithCtx :: (Set Int, Set Bool)
testPLiftWithCtx = plift Set.singleton (1::Int, True)

{-|
>>> testPLiftM
Identity ([1],[True])
-}
testPLiftM :: Identity ([Int], [Bool])
testPLiftM = pliftM (Identity . pure) (1::Int, True)

{-|
>>> testPDown
(1,True)
-}
testPDown :: (Int, Bool)
testPDown = pdown head ([1], [True])

{-|
>>> testPDownM
Identity (1,True)
-}
testPDownM :: Identity (Int, Bool)
testPDownM = pdownM (Identity . head) ([1::Int], [True])

{-|
>>> testPDown'
[(1,True),(1,False),(2,True),(2,False)]
-}
testPDown' :: [(Int, Bool)]
testPDown' = pdown' ([1, 2], [True, False])
