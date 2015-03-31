{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pack where

import           Control.Applicative (Applicative, WrappedMonad (..), (<$>))
import qualified Data.Foldable       as Foldable
import           Data.Generics
import           Data.Traversable    (Traversable)
import qualified Data.Traversable    as Traversable

class ContainerMap c where
  cmap  :: (forall a. Data a => t a -> t' a) -> c t -> c t'
  cmapA :: Applicative f =>
           (forall a. Data a => t a -> f (t' a)) -> c t -> f (c t')
  cmapM :: Monad m =>
           (forall a. Data a => t a -> m (t' a)) -> c t -> m (c t')
  cmapM f = unwrapMonad . cmapA (WrapMonad . f)
  fromContainer :: (forall a. Data a => t a -> t') -> c t -> [t']

class ContainerLift c c' where
  cup :: (forall a. a -> t a) -> c' -> c t
  cdown :: (forall a. t a -> a) -> c t -> c'

-- | Container to hold data (variable domain, reference,
-- assignment, etc.) related to variables.
class (ContainerMap c, ContainerLift c c') => Container c c'

-- | (for internal use)
newtype CTraversable t' v t =
  CTraversable { unCTraversable :: t' (t v) } deriving (Eq, Show)

instance (Data v, Traversable t') =>
         ContainerMap (CTraversable t' v) where
  cmap f (CTraversable ts) = CTraversable $ fmap f ts
  cmapA f (CTraversable ts) = CTraversable <$> Traversable.traverse f ts
  fromContainer f (CTraversable ts) = Foldable.toList $ fmap f ts

instance Traversable t' => ContainerLift (CTraversable t' v) (t' v) where
  cup f ts = CTraversable $ fmap f ts
  cdown f (CTraversable ts) = fmap f ts

instance (ContainerMap (CTraversable t' v),
          ContainerLift (CTraversable t' v) (t' v)) =>
         Container (CTraversable t' v) (t' v)
