-- Example of Container for Multiple Types

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Container2 where

import Control.Applicative (Applicative, WrappedMonad (..))
import Control.Applicative ((<$>), (<*>))
import Data.Foldable       (Foldable)
import Data.Maybe          (fromMaybe, listToMaybe, maybeToList)
import Data.Traversable    (Traversable, traverse)


newtype Pair a b = Pair (a, b) deriving (Show, Eq, Ord)

class C c t where
  type L c (t :: * -> *)
  cup   :: (forall a. a   -> t a ) -> c     -> L c t
  cdown :: (forall a. t a -> a   ) -> L c t -> c
  cmap  :: (forall a. t a -> t' a) -> L c t -> L c t'

class CMap c where
  type LMap c (t  :: * -> *)
--   cmap' :: (forall a. t a -> t' a) -> L' c t -> L' c t'
--  cmap' :: (a -> b) -> L' c t -> L' c t'
  cmap' :: (forall a. t a -> t' a) -> LMap c t -> LMap c t'
  cmapA :: Applicative f => (forall a. t a -> f (t' a))
        -> LMap c t -> f (LMap c t')

instance C (Pair a b) t where
  type L (Pair a b) t = Pair (t a) (t b)
  cup   f (Pair (a, b)) = Pair (f a, f b)
  cdown f (Pair (a, b)) = Pair (f a, f b)
  cmap  f (Pair (a, b)) = Pair (f a, f b)

instance CMap (Pair a b) where
  type LMap (Pair a b) t = Pair (t a) (t b)
  cmap' f (Pair (a, b)) = Pair (f a, f b)
  cmapA f (Pair (tx, ty)) = (\x y -> Pair (x,y)) <$> (f tx) <*> (f ty)

testUp :: Pair (Maybe Int) (Maybe Bool)
testUp = cup Just (Pair (1, True))

testDown :: Pair Int Bool
testDown = cdown (head . maybeToList) (Pair (Just 1, Just True))

-- cm :: (forall a. t a -> t' a) -> Pair (t a) (t b) -> Pair (t' a) (t' b)
-- cm = cmapA
-- testMap = cmap' maybeToList (Pair (Just 1, Just True))

-- testMap :: Pair [Int] [Bool]
-- testMap :: (C c Maybe, C c [], Num a, L c Maybe ~ Pair (Maybe a) (Maybe Bool), L c [] ~ Pair [a] [Bool]) => L c []
--mTL ::
-- testMap = cmap maybeToList (Pair (Just 1, Just True))


{-
class CLift c t where
  type L c (t :: * -> *)
  cup   :: (forall a. a -> t a) -> c -> L c t
  cdown :: (forall a. t a -> a) -> L c t -> c

class CMap c t where
  type L' c (t :: * -> *)
  cmap :: (forall a. t a -> t' a) -> L' c t -> L' c t'

instance CLift (a, b) t where
  type L (a, b) t = (t a, t b)
  cup   f (a, b) = (f a, f b)
  cdown f (a, b) = (f a, f b)

instance CLift (Pair a b) t where
  type L (Pair a b) t = Pair (t a) (t b)
  cup   f (Pair (a, b)) = Pair (f a, f b)
  cdown f (Pair (a, b)) = Pair (f a, f b)

instance CMap (Pair a b) t where
  type L' (Pair a b) t = Pair (t a) (t b)
  cmap f (Pair (a, b)) = Pair (f a, f b)
-}

class C' c where
  type B c
  bup   :: (forall a. a   -> t a ) -> B c -> c t
  bdown :: (forall a. t a -> a   ) -> c t -> B c
  bmap  :: (forall a. t a -> t' a) -> c t -> c t'

newtype Pair' a b t = Pair' (t a, t b) deriving (Show, Eq, Ord)

instance C' (Pair' a b) where
  type B (Pair' a b) = Pair a b
  bup   f (Pair  (a, b)) = Pair' (f a, f b)
  bdown f (Pair' (a, b)) = Pair  (f a, f b)
  bmap  f (Pair' (a, b)) = Pair' (f a, f b)

testUp' :: Pair' Int Bool Maybe
testUp' = bup Just (Pair (1, True))

testDown' :: Pair Int Bool
testDown' = bdown (head . maybeToList) (Pair' (Just 1, Just True))

testMap' :: Pair' Int Bool []
testMap' = bmap maybeToList (Pair' (Just 1, Just True))
