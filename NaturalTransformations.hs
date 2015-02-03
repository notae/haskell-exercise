-- | Examples for Natural Transformations

{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module NaturalTransformations where

import Text.Show.Functions ()

newtype NT f g = NT { runNT :: forall a. f a -> g a }
deriving instance Show (NT f g)

newtype ForLift f a = ForLift { getForLift :: a }
