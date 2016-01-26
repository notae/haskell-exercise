-- Example of Container for Multiple Types

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Container3 where

import Control.Applicative (Applicative, WrappedMonad (..))
import Control.Applicative ((<$>), (<*>))
import Data.Foldable       (Foldable)
import Data.Maybe          (fromMaybe, listToMaybe, maybeToList)
import Data.Traversable    (Traversable, traverse)

import qualified Data.Set as Set

class PackLift b p where
  pup   :: (forall a. a   -> t a ) -> b   -> p t
  pdown :: (forall a. t a -> a   ) -> p t -> b

class PackMap p where
  pmap  :: (forall a. t a -> t' a) -> p t  -> p t'
  pmapA :: Applicative f =>
           (forall a. t a -> f (t' a)) -> p t -> f (p t')
  pmapM :: Monad m =>
           (forall a. t a -> m (t' a)) -> p t -> m (p t')
  pmapM f = unwrapMonad . pmapA (WrapMonad . f)
  toList :: (forall a. t a -> t') -> p t -> [t']

class (PackMap p, PackLift b p) => Pack b p

{-
- parameterized : data T_ a b ...   = T_ a b
- applied       : type T            = T_ Int Bool ...
- lifted        : type TL a b ... t = T_ (t a) (t b) ...
- wrapped       : newtype TL'       = TL' (TL a b ... t)
-}
{-
-- parameterized
data    T_  a b   = T_ a     b
-- applied
type    T         = T_ Int   Bool
-- lifted
type    TL  a b t = T_ (t a) (t b)
-- wrapped
newtype TL' a b t = TL' (TL a b t)
-}
-- parameterized
data    T_  a b = T_ a     b           deriving (Show, Eq)
-- applied
type    T       = T_ Int   Bool
-- lifted
type    TL  t   = T_ (t Int) (t Bool)
-- wrapped
newtype TL' t   = TL' (TL t)           -- deriving (Show, Eq)
deriving instance (Eq (t Int), Eq (t Bool)) => Eq (TL' t)
deriving instance (Show (t Int), Show (t Bool)) => Show (TL' t)

newtype Pair  a b   = Pair  (  a,   b) deriving (Show, Eq)
newtype Pair' a b t = Pair' (t a, t b) deriving (Show, Eq)

instance PackLift (Pair a b) (Pair' a b) where
  pup   f (Pair  (a, b)) = Pair' (f a, f b)
  pdown f (Pair' (a, b)) = Pair  (f a, f b)

instance PackMap (Pair' a b) where
  pmap  f (Pair' (a, b)) = Pair' (f a, f b)
  pmapA f (Pair' (a, b)) = (\x y -> Pair' (x, y)) <$> (f a) <*> (f b)
  toList f (Pair' (a, b)) = [f a, f b]

instance Pack (Pair a b) (Pair' a b)

testUp :: Pair' Int Bool Maybe
testUp = pup Just (Pair (1 :: Int, True))

testDown :: Pair Int Bool
testDown = pdown (head . maybeToList) (Pair' (Just (1 :: Int), Just True))

testMap :: Pair' Int Bool []
testMap = pmap maybeToList (Pair' (Just 1, Just True))

-- data Showable = forall a. Show a => S a
data Showable = forall a. S a
-- deriving instance Show Showable

testToList :: [Showable]
testToList = toList S (Pair' (Just (1 :: Int), Just True))

instance PackLift (a, b) (Pair' a b) where
  pup   f (a, b)         = Pair' (f a, f b)
  pdown f (Pair' (a, b)) = (f a, f b)

instance Pack (a, b) (Pair' a b)


newtype Pair'' a b t = Pair'' (Pair (t a) (t b)) deriving (Show, Eq)

instance PackLift (Pair a b) (Pair'' a b) where
  pup   f (Pair         (a, b))  = Pair'' $ Pair (f a, f b)
  pdown f (Pair'' (Pair (a, b))) = Pair          (f a, f b)

instance PackMap (Pair'' a b) where
  pmap  f (Pair'' (Pair (a, b))) = Pair'' (Pair (f a, f b))
  pmapA f (Pair'' (Pair (a, b))) =
    (\x y -> Pair'' (Pair (x, y))) <$> (f a) <*> (f b)
  toList f (Pair'' (Pair (a, b))) = [f a, f b]

instance Pack (Pair a b) (Pair'' a b)


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
