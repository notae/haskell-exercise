-- | Examples for Natural Transformations

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NaturalTransformations where

import Data.Functor.Compose
import Data.Functor.Identity
import Text.Show.Functions   ()

newtype NT f g = NT { runNT :: forall a. f a -> g a }
deriving instance Show (NT f g)

pmapF :: Functor p => NT f g -> p (f a) -> p (g a)
pmapF f = fmap (runNT f)

class Pack p where
  pmap :: NT f g -> p f -> p g

-- Example data types
-- with multiple parameterized types
data P_ a b = P_ (a, b) deriving (Show, Eq)
-- for normal use
type P = P_ Int Bool
-- with context
newtype P' f = P' (P_ (f Int) (f Bool))
deriving instance (Show (f Int), Show (f Bool)) => Show (P' f)
-- for traverse over multiple parameterized types
instance Pack P' where
  pmap nt (P' (P_ (i, b))) = P' (P_ (i', b')) where
    i' = (runNT nt) i
    b' = (runNT nt) b

testB :: P
testB = P_ (1, True)
testI :: P' Identity
testI = P' (P_ (Identity 1, Identity True))
ntJust :: NT Identity Maybe
ntJust = NT (Just . runIdentity)
pmapJust :: Pack p => p Identity -> p Maybe
pmapJust = pmap ntJust
test :: P' Maybe
test = pmapJust testI

{-
with type family
-}
