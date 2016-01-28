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

Lift to 'Applicative' with monadic transformation:

>>> pliftA (Identity . pure) (1::Int, True) :: Identity ([Int], [Bool])
Identity ([1],[True])

Unlift 'Applicative' (partial):

>>> punlift head ([1], [True]) :: (Int, Bool)
(1,True)

Unlift 'Applicative' with monadic transformation:

>>> punliftA (Identity . head) ([1::Int], [True]) :: Identity (Int, Bool)
Identity (1,True)

Evaluate each action in the structure and collect the results:

>>> psequence ([1, 2], [True, False]) :: [(Int, Bool)]
[(1,True),(1,False),(2,True),(2,False)]
-}

class P p where
  type L p (g :: * -> *)
  plift :: (forall a. a -> g a) -> p -> L p g
  pliftA :: Applicative f => (forall a. a -> f (g a)) -> p -> f (L p g)
  punlift :: (forall a. g a -> a) -> L p g -> p
  punliftA :: Applicative f => (forall a. g a -> f a) -> L p g -> f p
  psequence :: Applicative g => L p g -> g p
  -- Default implementation
  type L p g = g p
  default plift :: (forall a. a -> g a) -> p -> g p
  default pliftA :: Applicative f => (forall a. a -> f (g a)) -> p -> f (g p)
  default punlift :: (forall a. g a -> a) -> g p -> p
  default punliftA :: Applicative f => (forall a. g a -> f a) -> g p -> f p
  default psequence :: Applicative g => g p -> g p
  plift f a = f a
  pliftA f a = f a
  punlift f a = f a
  punliftA f a = f a
  psequence a = a

-- Using default implementation
instance P Bool
instance P Int

-- confilct with 'type L (a, b) g'
-- instance (P a, Traversable p) => P (p a) where
--   type L (p a) g = p (L a g)
--   plift f as = runIdentity $ traverse f as

instance P a => P [a] where
  type L [a] g = [L a g]
  plift f = fmap (plift f)
  pliftA f = traverse (pliftA f)
  punlift f = fmap (punlift f)
  punliftA f = traverse (punliftA f)
  psequence = traverse psequence

newtype Tuple2 a b = Tuple2 (a, b) deriving (Show, Eq)

{-
instance (P a, P b) => P (Tuple2 a b) where
  type L (Tuple2 a b) g = Tuple2 (L a g) (L b g)
  plift f (a, b) = (,) (plift f a) (plift f b)
  pliftA f (a, b) = (,) <$> pliftA f a <*> pliftA f b
  punlift f (a, b) = (,) (punlift f a) (punlift f b)
  punliftA f (a, b) = (,) <$> punliftA f a <*> punliftA f b
  psequence (a, b) = (,) <$> psequence a <*> psequence b
-}

instance (P a, P b) => P (a, b) where
  type L (a, b) g = (L a g, L b g)
  plift f (a, b) = (,) (plift f a) (plift f b)
  pliftA f (a, b) = (,) <$> pliftA f a <*> pliftA f b
  punlift f (a, b) = (,) (punlift f a) (punlift f b)
  punliftA f (a, b) = (,) <$> punliftA f a <*> punliftA f b
  psequence (a, b) = (,) <$> psequence a <*> psequence b


newtype Lift g c = Lift (forall a. c a => a -> g a)
newtype LiftM g c m = LiftM (forall a. c a => a -> m (g a))
newtype Unlift g c = Unlift (forall a. c a => g a -> a)
newtype UnliftM g c m = UnliftM (forall a. c a => g a -> m a)

{-|
Lift to non-'Applicative' with type context ('Ord'):

>>> plift' ((Lift $ \x -> Set.fromList [x]) :: Lift Set Ord) (1::Int, True) :: (Set Int, Set Bool)
(fromList [1],fromList [True])

Lift to 'Applicative' with type context and monadic transformation:

>>> pliftA' ((LiftM $ Identity . Set.fromList . (:[])) :: LiftM Set Ord Identity) (1::Int, True) :: Identity (Set Int, Set Bool)
Identity (fromList [1],fromList [True])

Unlift 'Applicative' (partial) with type context:

>>> punlift' (Unlift minimum :: Unlift [] Ord) ([1::Int], [True]) :: (Int, Bool)
(1,True)

Unlift 'Applicative' with type context and monadic transformation:

>>> punliftA' ((UnliftM $ Identity . minimum) :: UnliftM [] Ord Identity) ([1::Int], [True]) :: Identity (Int, Bool)
Identity (1,True)
-}

class P' p c where
  plift' :: Lift g c -> p -> L p g
  pliftA' :: Applicative f => LiftM g c f -> p -> f (L p g)
  punlift' :: Unlift g c -> L p g -> p
  punliftA' :: Applicative f => UnliftM g c f -> L p g -> f p
  -- Default implementation
  default plift' :: c p => Lift g c -> p -> g p
  default pliftA' :: (c p, Applicative f) => LiftM g c f -> p -> f (g p)
  default punlift' :: c p => Unlift g c -> g p -> p
  default punliftA' :: (c p, Applicative f) => UnliftM g c f -> g p -> f p
  plift' (Lift f) a = f a
  pliftA' (LiftM f) a = f a
  punlift' (Unlift f) a = f a
  punliftA' (UnliftM f) a = f a

-- Using default implementation
instance c Bool => P' Bool c
instance c Int => P' Int c

instance (P' a c, P' b c) => P' (a, b) c where
  plift' f (a, b) = (,) (plift' f a) (plift' f b)
  pliftA' f (a, b) = (,) <$> pliftA' f a <*> pliftA' f b
  punlift' f (a, b) = (,) (punlift' f a) (punlift' f b)
  punliftA' f (a, b) = (,) <$> punliftA' f a <*> punliftA' f b


infixr 0 ~~>
type (f ~~> g) c = forall a. c a => f a -> g a

infixr 0 :~~>, $$$
newtype (f :~~> g) c = CNat { ($$$) :: (f ~~> g) c }


infixr 0 ~->
type (f ~-> g) c m = forall a. c a => f a -> m (g a)

infixr 0 :~->, $-
newtype (f :~-> g) c m = CNT { ($-) :: (f ~-> g) c m }


{-|
Lift to 'Applicative':

>>> p2lift (pure :: a -> [a]) (1::Int, True) :: ([Int], [Bool])
([1],[True])
-}
{-|
Lift to non-'Applicative':

>>> p2lift Set.singleton (1::Int, True) :: (Set Int, Set Bool)
(fromList [1],fromList [True])
-}
{-|
Lift nested structure:

>>> p2lift (pure :: a -> [a]) [(1::Int, True), (2, False)] :: [([Int], [Bool])]
[([1],[True]),([2],[False])]
-}
{-|
Lift to 'Applicative' with monadic transformation:

>>> p2liftA (Identity . pure :: a -> Identity [a]) (1::Int, True) :: Identity ([Int], [Bool])
Identity ([1],[True])
-}
{-|
Unlift 'Applicative' (partial):

>>> p2unlift head ([1:: Int], [True]) :: (Int, Bool)
(1,True)
-}
{-|
Unlift 'Applicative' with monadic transformation:

>>> p2unliftA (Identity . head) ([1::Int], [True]) :: Identity (Int, Bool)
Identity (1,True)
-}
{-|
Evaluate each action in the structure and collect the results:

>>> p2sequence ([1::Int, 2], [True, False]) :: [(Int, Bool)]
[(1,True),(1,False),(2,True),(2,False)]
-}

class P2 s t g where
  p2lift :: (forall a. a -> g a) -> s -> t
  p2lift f = runIdentity .  p2liftA (Identity . f)
  p2liftA :: Applicative f => (forall a. a -> f (g a)) -> s -> f t
  p2unlift :: (forall a. g a -> a) -> t -> s
  p2unlift f = runIdentity . p2unliftA (Identity . f)
  p2unliftA :: Applicative f => (forall a. g a -> f a) -> t -> f s
  p2sequence :: Applicative g => t -> g s

instance P2 a (g a) g where
  p2liftA f a = f a
  p2unliftA f a = f a
  p2sequence a = a

instance (P2 a a' g, Traversable t) => P2 (t a) (t a') g where
  p2liftA f = traverse (p2liftA f)
  p2unliftA f = traverse (p2unliftA f)
  p2sequence = traverse p2sequence

instance (P2 a a' g, P2 b b' g) => P2 (a, b) (a', b') g where
  p2liftA f (a, b) = (,) <$> (p2liftA f a) <*> (p2liftA f b)
  p2unliftA f (a, b) = (,) <$> (p2unliftA f a) <*> (p2unliftA f b)
  p2sequence (a, b) = (,) <$> p2sequence a <*> p2sequence b
