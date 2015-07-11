{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

module Pack4 where

import Control.Applicative
import Data.Dynamic
import Data.Functor.Identity
import Data.Maybe

import qualified Data.Foldable    as F
import qualified Data.Traversable as T

-- Types

class Applicative f => HasLift b l f where
  lift :: f b -> l
  unlift :: l -> f b

-- class GFunctor s t where
--   gmapA :: Applicative m => (forall a b. a -> m b) -> s -> m t
--   gmap :: (forall a b. a -> b) -> s -> t
--   gmap f = runIdentity . gmapA (Identity . f)

class HasNT s t f g where
  ntA :: Applicative m => (forall a. f a -> m (g a)) -> s -> m t
  nt :: (forall a. f a -> g a) -> s -> t
  nt f = runIdentity . ntA (Identity . f)

class ToList s g where
  toList :: (forall x. x -> g) -> s -> [g]

class ToDynList s where
  toDynList :: s -> [Dynamic]

-- Examples

testNT :: HasNT s t [] Maybe => s -> t
testNT = nt listToMaybe

-- Instances

--   Traversable

instance (T.Traversable t, Applicative f) => HasLift (t a) (t (f a)) f where
  unlift t = T.traverse id t

-- instance Traversable t => GFunctor (t a) (t b) where
--   gmapA f t = traverse f t

instance T.Traversable t => HasNT (t (f a)) (t (g a)) f g where
  ntA f t = T.traverse f t

instance (T.Traversable t, Typeable a) => ToList (t a) g where
  toList f t = fmap f (F.toList t)

instance (T.Traversable t, Typeable a) => ToDynList (t a) where
  toDynList t = fmap toDyn (F.toList t)

--   User-defined type

data PT i b = PT { _int :: i, _bool :: b } deriving (Show, Eq)

instance Applicative f => HasLift (PT i b) (PT (f i) (f b)) f where
  -- lift (PT i b) = PT (pure i) (pure b)
  unlift (PT i b) = PT <$> i <*> b

instance HasNT (PT (f i) (f b)) (PT (g i) (g b)) f g where
  ntA f (PT i b) = PT <$> (f i) <*> (f b)

instance ToList (PT i b) g where
  toList f (PT i b) = [f i, f b]

instance (Typeable i, Typeable b) => ToDynList (PT i b) where
  toDynList (PT i b) = [toDyn i, toDyn b]

testUniftPT :: PT [Int] [Bool] -> [PT Int Bool]
testUniftPT = unlift

testNTPT :: PT [Int] [Bool] -> PT (Maybe Int) (Maybe Bool)
testNTPT = nt listToMaybe

pt1 :: PT [Int] [Bool]
pt1 = PT [1::Int] [True]
