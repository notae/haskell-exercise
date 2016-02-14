{-# LANGUAGE ImpredicativeTypes #-}

module SBVTest5 where

import Control.Lens
import Data.Default
import Data.SBV

type T = (Int8, Int8)

app :: Functor f => s -> (Lens' s a, f a) -> f s
app s (l, a) = s & l %%~ const a

{-|
>>> test1
[(1,3),(1,4)]
-}
test1 :: [T]
test1 = app (1, def) (_2, [3, 4])
