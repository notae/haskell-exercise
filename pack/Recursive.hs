{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Recursive where

import Data.Functor.Identity
import Text.Show.Functions   ()

import           Data.Set (Set)
import qualified Data.Set as Set


{-|
Lift to 'Applicative':

>>> plift pure (1::Int, True) :: ([Int], [Bool])
([1],[True])

Lift to non-'Applicative':

>>> plift Set.singleton (1::Int, True) :: (Set Int, Set Bool)
(fromList [1],fromList [True])

>>> pliftM (Identity . pure) (1::Int, True) :: Identity ([Int], [Bool])
Identity ([1],[True])

>>> punlift head ([1], [True]) :: (Int, Bool)
(1,True)

>>> punliftM (Identity . head) ([1::Int], [True]) :: Identity (Int, Bool)
Identity (1,True)

>>> psequence ([1, 2], [True, False]) :: [(Int, Bool)]
[(1,True),(1,False),(2,True),(2,False)]
-}

class P p where
  type L p (f :: * -> *)
  plift :: (forall a. a -> f a) -> p -> L p f
  pliftM :: Monad m => (forall a. a -> m (f a)) -> p -> m (L p f)
  punlift :: (forall a. f a -> a) -> L p f -> p
  punliftM :: Monad m => (forall a. f a -> m a) -> L p f -> m p
  psequence :: Applicative f => L p f -> f p
  -- Default implementation
  type L p f = f p
  default plift :: (forall a. a -> f a) -> p -> f p
  default pliftM :: Monad m => (forall a. a -> m (f a)) -> p -> m (f p)
  default punlift :: (forall a. f a -> a) -> f p -> p
  default punliftM :: Monad m => (forall a. f a -> m a) -> f p -> m p
  default psequence :: Applicative f => f p -> f p
  plift f a = f a
  pliftM f a = f a
  punlift f a = f a
  punliftM f a = f a
  psequence a = a

-- Using default implementation
instance P Bool
instance P Int

instance (P a, P b) => P (a, b) where
  type L (a, b) f = (L a f, L b f)
  plift f (a, b) = (plift f a, plift f b)
  pliftM f (a, b) = (,) <$> pliftM f a <*> pliftM f b
  punlift f (a, b) = (punlift f a, punlift f b)
  punliftM f (a, b) = (,) <$> punliftM f a <*> punliftM f b
  psequence (a, b) = (,) <$> psequence a <*> psequence b


newtype Lift f c = Lift (forall a. c a => a -> f a)
newtype LiftM f c m = LiftM (forall a. c a => a -> m (f a))
newtype Unlift f c = Unlift (forall a. c a => f a -> a)
newtype UnliftM f c m = UnliftM (forall a. c a => f a -> m a)

{-|
Lift to non-'Applicative' with type context ('Ord'):

>>> plift' ((Lift $ \x -> Set.fromList [x]) :: Lift Set Ord) (1::Int, True) :: (Set Int, Set Bool)
(fromList [1],fromList [True])

>>> pliftM' ((LiftM $ Identity . Set.fromList . (:[])) :: LiftM Set Ord Identity) (1::Int, True) :: Identity (Set Int, Set Bool)
Identity (fromList [1],fromList [True])

>>> punlift' (Unlift minimum :: Unlift [] Ord) ([1::Int], [True]) :: (Int, Bool)
(1,True)

>>> punliftM' ((UnliftM $ Identity . minimum) :: UnliftM [] Ord Identity) ([1::Int], [True]) :: Identity (Int, Bool)
Identity (1,True)
-}

class P' p c where
  plift' :: Lift f c -> p -> L p f
  pliftM' :: Monad m => LiftM f c m -> p -> m (L p f)
  punlift' :: Unlift f c -> L p f -> p
  punliftM' :: Monad m => UnliftM f c m -> L p f -> m p
  -- Default implementation
  default plift' :: c p => Lift f c -> p -> f p
  default pliftM' :: (c p, Monad m) => LiftM f c m -> p -> m (f p)
  default punlift' :: c p => Unlift f c -> f p -> p
  default punliftM' :: (c p, Monad m) => UnliftM f c m -> f p -> m p
  plift' (Lift f) a = f a
  pliftM' (LiftM f) a = f a
  punlift' (Unlift f) a = f a
  punliftM' (UnliftM f) a = f a

-- Using default implementation
instance c Bool => P' Bool c
instance c Int => P' Int c

instance (P' a c, P' b c) => P' (a, b) c where
  plift' f (a, b) = (plift' f a, plift' f b)
  pliftM' f (a, b) = (,) <$> pliftM' f a <*> pliftM' f b
  punlift' f (a, b) = (punlift' f a, punlift' f b)
  punliftM' f (a, b) = (,) <$> punliftM' f a <*> punliftM' f b


infixr 0 ~~>
type (f ~~> g) c = forall a. c a => f a -> g a

infixr 0 :~~>, $$$
newtype (f :~~> g) c = CNat { ($$$) :: (f ~~> g) c }


infixr 0 ~->
type (f ~-> g) c m = forall a. c a => f a -> m (g a)

infixr 0 :~->, $-
newtype (f :~-> g) c m = CNT { ($-) :: (f ~-> g) c m }
