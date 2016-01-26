-- Example of Container for Multiple Types

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Container where

import Control.Applicative (Applicative, WrappedMonad (..))
import Control.Applicative ((<$>), (<*>))
import Data.Foldable       (Foldable)
import Data.Maybe          (fromMaybe, listToMaybe, maybeToList)
import Data.Traversable    (Traversable, traverse)

import qualified Data.Set as Set

class Pack p where
  type Base p
  pup   :: (forall a. a   -> t a ) -> Base p -> p t
  pdown :: (forall a. t a -> a   ) -> p t    -> Base p
  pmap  :: (forall a. t a -> t' a) -> p t    -> p t'
  pmapA :: Applicative f =>
           (forall a. t a -> f (t' a)) -> p t -> f (p t')
  pmapM :: Monad m =>
           (forall a. t a -> m (t' a)) -> p t -> m (p t')
  pmapM f = unwrapMonad . pmapA (WrapMonad . f)
  toList :: (forall a. t a -> t') -> p t -> [t']

newtype Pair  a b   = Pair  (  a,   b) deriving (Show, Eq)
newtype Pair' a b t = Pair' (t a, t b) deriving (Show, Eq)

instance Pack (Pair' a b) where
  type Base (Pair' a b) = Pair a b
  pup   f (Pair  (a, b)) = Pair' (f a, f b)
  pdown f (Pair' (a, b)) = Pair  (f a, f b)
  pmap  f (Pair' (a, b)) = Pair' (f a, f b)
  pmapA f (Pair' (a, b)) = (\x y -> Pair' (x, y)) <$> (f a) <*> (f b)
  toList f (Pair' (a, b)) = [f a, f b]

testUp :: Pair' Int Bool Maybe
testUp = pup Just (Pair (1, True))

testDown :: Pair Int Bool
testDown = pdown (head . maybeToList) (Pair' (Just 1, Just True))

testMap :: Pair' Int Bool []
testMap = pmap maybeToList (Pair' (Just 1, Just True))

-- data Showable = forall a. Show a => S a
data Showable = forall a. S a
-- deriving instance Show Showable

testToList :: [Showable]
testToList = toList S (Pair' (Just 1, Just True))

type family Up a
-- type instance Up [a] = forall a. Show a => a -> [a]
type instance Up [a] = a -> [a]

func :: Up [a]
func a = [a]

newtype PTraversable t' v t =
  PTraversable { unPTraversable :: t' (t v) } deriving (Show, Eq)

{-
instance Traversable t' =>
         P (PTraversable t' v) where
  pmapA f (PTraversable ts) = PTraversable <$> Traversable.traverse f ts
  fromContainer f (PTraversable ts) = Foldable.toList $ fmap f ts

instance Traversable t' => ContainerLift (PTraversable t' v) (t' v) where
  pup f ts = PTraversable $ fmap f ts
  pdown f (PTraversable ts) = fmap f ts
-}
