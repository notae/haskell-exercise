{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Pack10 where

import Data.Functor.Identity
import Data.Maybe
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

{-
instance P a where
  type L a f = f a
  plift f a = f a
  pliftM f a = f a
  pdown f a = f a
  pdown' a = a
-}

instance P Bool where
  type L Bool f = f Bool
  plift f a = f a
  pliftM f a = f a
  pdown f a = f a
  pdownM f a = f a
  pdown' a = a

instance P Int where
  type L Int f = f Int
  plift f a = f a
  pliftM f a = f a
  pdown f a = f a
  pdownM f a = f a
  pdown' a = a

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
