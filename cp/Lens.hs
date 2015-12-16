module Lens where

import Control.Lens

data P a = P String a deriving Show
val :: Lens (P a) (P b) a b
val f (P k v) = fmap (\b -> P k b) (f v)

-- liftPV :: Applicative f => P a -> P (f a)
-- liftPV = val %~ pure

-- liftL :: Applicative f => Lens s t a b -> s -> t
-- liftL l = over l pure
