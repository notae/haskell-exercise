{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Pack3 where

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid

import qualified Data.Foldable    as F
import qualified Data.Traversable as T

class PackLift b p where
  lift   :: Applicative f => b   -> p f
  unlift :: Applicative f => p f -> f b

class PackNT p where
  ntA  :: Applicative f => (forall x. g x -> f (h x)) -> p g -> f (p h)
  nt   :: (forall x. g x -> h x) -> p g -> p h
  nt f = runIdentity . ntA (Identity . f)

class PackFold p where
  toDynList :: p -> [Dynamic]

type Pack b p f = (PackLift b p, PackNT p, PackFold (p f))

-- Instances for Traversable

newtype WrappedTraversable t a f =
  WrapTraversable { unwrapTraversable :: t (f a) }
  deriving Show

instance T.Traversable t => PackLift (t a) (WrappedTraversable t a) where
  lift = WrapTraversable . fmap pure
  unlift (WrapTraversable t) = T.traverse id t

instance (T.Traversable t) => PackNT (WrappedTraversable t a) where
  ntA f (WrapTraversable t) = WrapTraversable <$> T.traverse f t

instance (T.Traversable t, Typeable a, Typeable f) =>
         PackFold (WrappedTraversable t a f) where
  toDynList (WrapTraversable t) = fmap toDyn (F.toList t)

-- Instances for (a, b)

newtype PairL a b f = PairL (f a, f b) deriving (Show, Eq)

instance PackLift (a, b) (PairL a b) where
  lift (a, b) = PairL (pure a, pure b)
  unlift (PairL (a, b)) = (,) <$> a <*> b

instance PackNT (PairL a b) where
  ntA f (PairL (a, b)) = PairL <$> ((,) <$> f a <*> f b)

instance (Typeable a, Typeable b, Typeable f) => PackFold (PairL a b f) where
  toDynList (PairL (a, b)) = [toDyn a, toDyn b]

pl1 :: PairL Int Bool []
pl1 = PairL ([1, 2], [True, False])
