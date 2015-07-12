{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Pack5 where

import Control.Applicative
import Control.Lens
import Control.Monad.ST.Lazy
import Data.Dynamic
import Data.Functor.Identity
import Data.Maybe
import Data.STRef.Lazy

import qualified Data.Foldable    as F
import qualified Data.Traversable as T

-- Interface
{-
forall a.
  a    a
  f    g
  s -> t -> b
       m    m
-}

-- type C a = (Show a, Ord a)
type C a = Typeable a

type ToList t = forall g. (forall a. C a => a -> g) -> t -> [g]

type GNT s t f g =
  forall m. Applicative m => (forall a. C a => f a -> m (g a)) -> s -> m t

type GUnlift t b g = Applicative g => t -> g b

type GUnlift' s b f g =
  (Applicative m, Applicative g) => (forall a. C a => f a -> m (g a)) -> s -> m (g b)

class HasList t where
  toList :: ToList t

class HasGNT s t f g where
  gnt :: GNT s t f g

class HasUnlift t b g where
  gunlift :: GUnlift t b g

class HasUnlift' s b f g where
  gunlift' :: GUnlift' s b f g

-- Instances

--   Traversable

instance (F.Foldable t, C a) => HasList (t a) where
  toList f = fmap f . F.toList

instance (T.Traversable t, C a) => HasGNT (t (f a)) (t (g a)) f g where
  gnt f = traverse f

instance (T.Traversable t, Applicative f) => HasUnlift (t (f a)) (t a) f where
  gunlift = traverse id

instance (T.Traversable t, Applicative f, C a) => HasUnlift' (t (f a)) (t a) f g where
  -- gunlift' f = liftA gunlift . gnt f
  gunlift' f = liftA gunlift . traverse f

{-|
>>> testNTTraversable
[[1],[2]]
-}
testNTTraversable :: [[Int]]
testNTTraversable = runIdentity $ gnt (Identity . maybeToList) [Just (1::Int), Just 2]

{-|
>>> testUnliftTraversable'
Just [[1,2]]
-}
testUnliftTraversable' :: Maybe [[Int]]
testUnliftTraversable' = gunlift' (Just . maybeToList) [Just (1::Int), Just 2]

--   (,)

instance (C a, C b) => HasList (a, b) where
  toList f (a, b) = [f a, f b]

ntTuple :: (C a, C b) => GNT (f a, f b) (g a, g b) f g
ntTuple f (a, b) = (,) <$> f a <*> f b

instance (C a, C b) => HasGNT (f a, f b) (g a, g b) f g where
  gnt = ntTuple

unliftTuple :: GUnlift (g a, g b) (a, b) g
unliftTuple (a, b) = (,) <$> a <*> b

instance HasUnlift (g a, g b) (a, b) g where
  gunlift = unliftTuple

unliftTuple' :: (C a, C b) => GUnlift' (f a, f b) (a, b) f g
unliftTuple' f = liftA unliftTuple . ntTuple f

instance (C a, C b) => HasUnlift' (f a, f b) (a, b) f g where
  gunlift' = unliftTuple'

{-|
>>> testNTTuple
([1],[True])
-}
testNTTuple :: ([Int], [Bool])
testNTTuple = runIdentity $ gnt (Identity . maybeToList) (Just (1::Int), Just True)

{-|
>>> testUnliftTuple'
Just [(1,True)]
-}
testUnliftTuple' :: Maybe [(Int, Bool)]
testUnliftTuple' = gunlift' (Just . maybeToList) (Just (1::Int), Just True)

-- Usecase

data Var s a = Var { getVar :: STRef s [a] }

--   using type synonyms

newV :: GNT t' t [] (Var s) -> t' -> ST s t
newV nt = nt (fmap Var . newSTRef)

getV :: GNT t' t (Var s) [] -> t' -> ST s t
getV nt = nt (readSTRef . getVar)

testST :: ST s [(Int, Bool)]
testST = do
  v <- newV ntTuple ([1, 2], [True, False])
  modifySTRef (v^._1 & getVar) (fmap (+1))
  vs <- getV ntTuple v
  return $ unliftTuple vs

--   using type classes

newV' :: HasGNT t' t [] (Var s) => t' -> ST s t
newV' = gnt (fmap Var . newSTRef)

getV' :: HasGNT t' t (Var s) [] => t' -> ST s t
getV' = gnt (readSTRef . getVar)

getV'' :: HasUnlift' t' b (Var s) [] => t' -> ST s [b]
getV'' = gunlift' (readSTRef . getVar)

incV' :: Var s Int -> ST s ()
incV' v = modifySTRef (getVar v) (fmap succ)

testST' :: ST s [(Int, Bool)]
testST' = do
  v <- newV' ([1::Int, 2], [True, False]) :: ST s (Var s Int, Var s Bool)
  -- v <- newV' ([1::Int, 2], [True, False])
  incV' (v^._1)
  getV'' v
