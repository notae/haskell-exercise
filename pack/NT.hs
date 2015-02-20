-- | Examples for Natural Transformations

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module NT where

import Control.Applicative
import Control.Category      as Category
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Prelude               hiding (id, (.))
import Text.Show.Functions   ()

newtype NT f g = NT { runNT :: forall a. f a -> g a }
deriving instance Show (NT f g)

pmapF :: Functor p => NT f g -> p (f a) -> p (g a)
pmapF f = fmap (runNT f)

class Pack p where
  pmap :: (forall a. f a -> g a) -> p f -> p g
  pmap' :: NT f g -> p f -> p g

-- Example data types
-- with multiple parameterized types
data P_ a b = P_ (a, b) deriving (Show, Eq)
-- for normal use
type P = P_ Int Bool
-- with context
type    PL f =     P_ (f Int) (f Bool)
newtype P' f = P' (P_ (f Int) (f Bool))
deriving instance (Show (f Int), Show (f Bool)) => Show (P' f)
-- for traverse over multiple parameterized types
instance Pack P' where
  pmap f (P' (P_ (i, b))) = P' (P_ (f i, f b))
  pmap' nt (P' (P_ (i, b))) = P' (P_ (i', b')) where
    i' = (runNT nt) i
    b' = (runNT nt) b

testB :: P
testB = P_ (1, True)
testL :: PL []
testL = P_ ([1], [True])
testI :: P' Identity
testI = P' (P_ (Identity 1, Identity True))
ntJust :: NT Identity Maybe
ntJust = NT (Just . runIdentity)
pmapJust :: Pack p => p Identity -> p Maybe
pmapJust = pmap' ntJust
test :: P' Maybe
test = pmapJust testI

-- with type family
-- map base type to lifted type
type family L p :: (* -> *) -> *
class PackLift p where
  pup   :: Applicative f =>                         p     -> L p f
  pup = pup' pure
  pup'  :: Applicative f => (forall a. a -> f a) -> p     -> L p f
  pdown :: Applicative f => (forall a. f a -> a) -> L p f -> p

type instance L P = P'
instance PackLift P where
  pup' f (P_ (i, b)) = P' $ P_ (f i, f b)
  pdown f (P' (P_ (i, b))) = P_ (f i, f b)

{-
type instance L P = P'
instance PackLift P where
  pup' f (P_ (i, b)) = P' $ P_ (f i, f b)
  pdown f (P' (P_ (i, b))) = P_ (f i, f b)

{-|
>>> testPup
P' (P_ (Just 1,Just True))
-}
testPup :: L P Maybe
testPup = pup testB
-}

-- with type family (another pattern)
-- map lifted type to base type
type family B p :: *
class PackLift' p where
  plup  :: (forall a f. a -> f a) -> B p -> p
  plupA :: B p -> p
  plupA = plupA' pure
  plupA' :: (forall a f. Applicative f => a -> f a) -> B p -> p
  pldown :: (forall a f. f a -> a) -> p -> B p

type instance B (PL f) = P
instance Applicative f => PackLift' (PL f) where
  plup f (P_ (i, b)) = P_ (f i, f b)
  plupA' f (P_ (i, b)) = P_ (f i, f b)
  pldown f (P_ (i, b)) = P_ (f i, f b)

{-
{-|
>>> testPlup
P_ (Just 1,Just True)
-}
testPlup :: PL Maybe
testPlup = plup Just testB

{-|
>>> testPlupA
P_ (Just 1,Just True)
-}
testPlupA :: PL Maybe
testPlupA = plupA testB

{-|
>>> testPldown
P_ (1,True)
-}
testPldown :: P
testPldown = pldown head testL
-}
