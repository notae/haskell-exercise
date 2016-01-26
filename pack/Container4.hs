-- Example of Container for Multiple Types

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Container4 where

import Control.Applicative (Applicative, WrappedMonad (..))
import Control.Applicative ((<$>), (<*>))
import Data.Foldable       (Foldable)
import Data.Maybe          (fromMaybe, listToMaybe, maybeToList)
import Data.Traversable    (Traversable, traverse)

import qualified Data.Set as Set

type family L p (t :: * -> *)

class PackLift p where
  pup   :: (forall a. a   -> t a ) -> p     -> L p t
  pdown :: (forall a. t a -> a   ) -> L p t -> p

class PackMap p where
  pmap  :: (forall a. t a -> t' a) -> L p t  -> L p t'
  pmapA :: Applicative f =>
           (forall a. t a -> f (t' a)) -> L p t -> f (L p t')
  -- pmapM :: Monad m =>
  --          (forall a. t a -> m (t' a)) -> L p t -> m (L p t')
  -- pmapM f = unwrapMonad . pmapA (WrapMonad . f)
  toList :: (forall a. t a -> t') -> L p t -> [t']

class (PackMap p, PackLift p) => Pack p

newtype Pair  a b   = Pair  (  a,   b) deriving (Show, Eq)
-- newtype Pair' a b t = Pair' (t a, t b) deriving (Show, Eq)
type Pair' a b f = Pair (f a) (f b)

type instance L (Pair a b) t = Pair (t a) (t b)

instance PackLift (Pair a b) where
  pup   f (Pair (a, b)) = Pair (f a, f b)
  pdown f (Pair (a, b))  = Pair (f a, f b)

instance PackMap (Pair a b) where
  pmap  f (Pair (a, b)) = Pair (f a, f b)
  pmapA f (Pair (a, b)) = (\x y -> Pair (x, y)) <$> (f a) <*> (f b)
  toList f (Pair (a, b)) = [f a, f b]

testUp :: Pair' Int Bool Maybe
testUp = pup Just (Pair (1 :: Int, True))

testDown :: Pair Int Bool
testDown = pdown (head . maybeToList) (Pair (Just (1 :: Int), Just True))

-- NG: The type variable ‘p1’ is ambiguous
-- testMap :: Pair' Int Bool []
-- testMap = pmap maybeToList (Pair (Just 1, Just True))
-- testMap = (pmap :: (Maybe a -> [a]) -> Pair' Int Bool Maybe -> Pair' Int Bool []) maybeToList (Pair (Just 1, Just True))

-- data Showable = forall a. Show a => S a
data Showable = forall a. S a
-- deriving instance Show Showable

-- NG: The type variables ‘p0’, ‘t0’ are ambiguous
-- testToList :: [Showable]
-- testToList = toList S (Pair (Just (1 :: Int), Just True))

{-
instance PackLift (a, b) ( a b) where
  pup   f (Pair  (a, b)) = Pair' (f a, f b)
  pdown f (Pair' (a, b)) = Pair  (f a, f b)

instance PackMap (Pair' a b) where
  pmap  f (Pair' (a, b)) = Pair' (f a, f b)
  pmapA f (Pair' (a, b)) = (\x y -> Pair' (x, y)) <$> (f a) <*> (f b)
  toList f (Pair' (a, b)) = [f a, f b]
-}


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
