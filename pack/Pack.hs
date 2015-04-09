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

class PackMap p where
  pmap  :: (forall a. Data a => t a -> t' a) -> p t -> p t'
  pmapA :: Applicative f =>
           (forall a. Data a => t a -> f (t' a)) -> p t -> f (p t')
  pmapM :: Monad m =>
           (forall a. Data a => t a -> m (t' a)) -> p t -> m (p t')
  pmapM f = unwrapMonad . pmapA (WrapMonad . f)
  fromPack :: (forall a. Data a => t a -> t') -> p t -> [t']

class PackLift p p' where
  pup :: (forall a. a -> t a) -> p' -> p t
  pdown :: (forall a. t a -> a) -> p t -> p'

class (PackMap p, PackLift p p') => Pack p p'

--
-- Instances for Traversable
--

newtype PTraversable t' v t =
  PTraversable { unPTraversable :: t' (t v) } deriving (Eq, Show)

instance (Data v, Traversable t') =>
         PackMap (PTraversable t' v) where
  pmap f (PTraversable ts) = PTraversable $ fmap f ts
  pmapA f (PTraversable ts) = PTraversable <$> Traversable.traverse f ts
  fromPack f (PTraversable ts) = Foldable.toList $ fmap f ts

instance Traversable t' => PackLift (PTraversable t' v) (t' v) where
  pup f ts = PTraversable $ fmap f ts
  pdown f (PTraversable ts) = fmap f ts

instance (PackMap (PTraversable t' v),
          PackLift (PTraversable t' v) (t' v)) =>
         Pack (PTraversable t' v) (t' v)


--
-- Examples
--

data Color = Red | Green | Blue deriving (Show, Eq)
data D c n = D { dCol :: c, dNum :: n } deriving (Show, Eq)
d :: D (Maybe Color) [Int]
d = D (Just Green) [1..3]
ns :: [Int]
ns = (+1) <$> dNum d

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
