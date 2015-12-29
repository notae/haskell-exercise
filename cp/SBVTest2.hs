{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module SBVTest2 where

import Data.Generics
import Data.SBV
import GHC.Exts
import Data.Functor.Identity

type family Lift t (f :: * -> *)
type instance Lift (a, b) f = (f a, f b)

class Pack t t' where
  ptraverse :: Applicative f => (forall a b. a -> f b) -> t -> f t'

instance Pack (a, b) (a', b') where
  ptraverse f (a, b) = (,) <$> f a <*> f b
