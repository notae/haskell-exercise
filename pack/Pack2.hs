{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pack2 where

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Traversable

--
-- New Pack
--

class PackLift b p where
  pLiftUp   :: Applicative f => b   -> p f
  pLiftDown :: Applicative f => p f -> f b

class PackMap p where
  pNTM      :: Applicative f => (forall x. g x -> f (h x)) -> p g -> f (p h)
  pNT       :: (forall x. g x -> h x) -> p g -> p h
  pNT f     = runIdentity . pNTM (Identity . f)
  -- TBD: put x under constraints
  pToList   :: Applicative f => (forall x. f x -> y) -> p f -> [y]
  pToList'  :: (Applicative f, Typeable f) => p f -> [Dynamic]
  pFoldM    :: Applicative f => (forall x. s -> f x -> s) -> s -> p f -> s

type Pack b p = (PackLift b p, PackMap p)

-- Instances for Traversable

newtype WrappedTraversable t a f =
  WrapTraversable { unwrapTraversable :: t (f a) }
  deriving Show

instance Traversable t => PackLift (t a) (WrappedTraversable t a) where
  pLiftUp = WrapTraversable . fmap pure
  pLiftDown (WrapTraversable t) = traverse id t

instance (Traversable t, Typeable a) => PackMap (WrappedTraversable t a) where
  pNTM f (WrapTraversable t) = WrapTraversable <$> traverse f t
  pToList f (WrapTraversable t) = fmap f (toList t)
  pToList' (WrapTraversable t) = fmap toDyn (toList t)

-- Instances for (a, b)

newtype PairL a b f = PairL (f a, f b) deriving (Show, Eq)

instance PackLift (a, b) (PairL a b) where
  pLiftUp (a, b) = PairL (pure a, pure b)
  pLiftDown (PairL (a, b)) = (,) <$> a <*> b

instance (Typeable a, Typeable b) => PackMap (PairL a b) where
  pNTM f (PairL (a, b)) = PairL <$> ((,) <$> f a <*> f b)
  pToList f (PairL (a, b)) = [f a, f b]
  pToList' (PairL (a, b)) = [toDyn a, toDyn b]
  pFoldM f s0 (PairL (a, b)) = f (f s0 a) b

pl0 :: (Int, Bool)
pl0 = (1, True)

pl1 :: PairL Int Bool []
pl1 = PairL ([1], [True])

pl2 :: PairL Int Bool []
pl2 = PairL ([1, 2], [True])

pl3 :: PairL Int Bool []
pl3 = PairL ([1, 2], [True, False])

{-|
>>> testPLU
PairL ([1],[True])
-}
testPLU :: PairL Int Bool []
testPLU = pLiftUp pl0

{-|
>>> testPLD
[(1,True)]
>>> testPLD2
[(1,True),(1,False),(2,True),(2,False)]
-}
testPLD :: [(Int, Bool)]
testPLD = pLiftDown pl1
testPLD2 :: [(Int, Bool)]
testPLD2 = pLiftDown pl3

{-|
>>> testPNTM
PairL (Just 1,Just True)
-}
testPNTM :: PairL Int Bool Maybe
testPNTM = runIdentity $ pNTM (Identity . listToMaybe) pl1

{-|
>>> testFM
[1,2]
-}
testFM :: [Int]
testFM = pFoldM f mempty pl2 where
  f s x = length x : s


testPack :: Pack b p => b -> p []
testPack = pLiftUp

--
-- Experimental code
--

{-
traverse :: (Traversable t, Applicative f)
         => (a -> f b) -> t a -> f (t b)

usecase:
forall a.
t a         -> t (Dom a)            -- (1) Create domain with single value
t (Dom a)   -> m (t (Var a))        -- (2) Create new variables
t (Var a)   -> m (t (Dom a))        -- (3) Read values of variables
t (Dom a)   -> t (Maybe a)          -- (4) Convert Domain to Maybe
t (Maybe a) -> Maybe (t a)          -- (5) Convert Maybe to single value
-}

-- single type:
{-|
>>> st5 $ st4 $ st1 [1,2,3]
Just [1,2,3]
>>> st45 $ st1 [1,2,3]
Just [1,2,3]
-}
-- (1)
st1 :: (Traversable t, Applicative f) => t a -> t (f a)
st1 = runIdentity . traverse (Identity . pure)
-- (2)(3)
st2 :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
st2 = traverse
-- (4)
st4 :: (Traversable t) => t [a] -> t (Maybe a)
st4 = runIdentity . traverse (Identity . listToMaybe)
-- (5)
st5 :: (Traversable t) => t (Maybe a) -> Maybe (t a)
st5 = traverse id
-- (4)+(5)
st45 :: (Traversable t) => t [a] -> Maybe (t a)
st45 = traverse listToMaybe

-- ex.: t = (a, b), f = []
{-|
>>> plLiftDown' $ plTranslate' $ plLiftUp' (1,True)
Just (1,True)
>>> plLiftDown'' $ plLiftUp' (1,True)
Just (1,True)
-}
-- (1)
plLiftUp' :: (a, b) -> ([a], [b])
plLiftUp' (a, b) = ([a], [b])
-- (4)
plTranslate' :: ([a], [b]) -> (Maybe a, Maybe b)
plTranslate' (a, b) = (listToMaybe a, listToMaybe b)
-- (5)
plLiftDown' :: (Maybe a, Maybe b) -> Maybe (a, b)
plLiftDown' (a, b) = (,) <$> a <*> b
-- (4)+(5)
plLiftDown'' :: ([a], [b]) -> Maybe (a, b)
plLiftDown'' (a, b) = (,) <$> listToMaybe a <*> listToMaybe b

{-|
>>> pgLiftDown $ pgNT listToMaybe $ pgLiftUp (1,True)
Just (1,True)
>>> pgLiftDown $ pgNT' listToMaybe $ pgLiftUp (1,True)
Just (1,True)
>>> pgNTM (Just . listToMaybe) ([1,2],[True,False])
Just (Just 1,Just True)
>>> runIdentity $ pgNTM (Identity . listToMaybe) ([1,2],[True,False])
(Just 1,Just True)
-}
-- (1) primitive
pgLiftUp :: Applicative f => (a, b) -> (f a, f b)
pgLiftUp (a, b) = (pure a, pure b)
-- (2)(3) primitive
pgNTM :: Applicative f
    => (forall x. g x -> f (h x)) -> (g a, g b) -> f (h a, h b)
pgNTM f (a, b) = (,) <$> f a <*> f b
-- (4)
pgNT :: (forall x. f x -> g x) -> (f a, f b) -> (g a, g b)
pgNT f (a, b) = (f a, f b)
--   rewritten with primitive
pgNT' :: (forall x. f x -> g x) -> (f a, f b) -> (g a, g b)
pgNT' f = runIdentity . pgNTM (Identity . f)
-- (5) primitive
pgLiftDown :: Applicative f => (f a, f b) -> f (a, b)
pgLiftDown (a, b) = (,) <$> a <*> b

-- TBD: generalize on element type
-- multi type:
-- (1)
-- mt1 :: (forall a. a -> f a)   -> f a
-- (forall a. f a -> g a) ->
-- (forall a. f a -> a)   -> t f -> f t

-- (2)(3) primitive
mNTM :: Applicative f
     => (forall x. g x -> f (h x)) -> t g -> f (t h)
mNTM = undefined
