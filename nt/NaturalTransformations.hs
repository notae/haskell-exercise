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

-- example data type
data Pair_ a b = Pair_ (a, b) deriving (Show, Eq)
type Pair = Pair_ Int Bool
newtype Pair' f = Pair' (Pair_ (f Int) (f Bool))
deriving instance (Show (f Int), Show (f Bool)) => Show (Pair' f)
instance Pack Pair' where
  pmap nt (Pair' (Pair_ (i, b))) = Pair' (Pair_ (i', b')) where
    i' = (runNT nt) i
    b' = (runNT nt) b

testB :: Pair
testB = Pair_ (1, True)
testI :: Pair' Identity
testI = Pair' (Pair_ (Identity 1, Identity True))
ntJust :: NT Identity Maybe
ntJust = NT (Just . runIdentity)
pmapJust :: Pack p => p Identity -> p Maybe
pmapJust = pmap ntJust
test :: Pair' Maybe
test = pmapJust testI
