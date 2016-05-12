{-# LANGUAGE ImpredicativeTypes #-}

module SBVTest5 where

import Control.Lens
import Control.Monad
import Data.Default
import Data.SBV

type T = (Int8, Int8)

app :: Monad f => [(Lens' s a, f a)] -> s -> f s
app ls s0 = foldM go s0 ls where
  go :: Functor f => s -> (Lens' s a, f a) -> f s
  go s (l, a) = (l %%~ const a) s

{-|
>>> test1
[(1,3),(1,4)]
-}
test1 :: [T]
test1 = app [(_2, [3, 4])] (1, def)


{-
--
-- Lens Experiments
--

sTuple :: (SymWord a, SymWord b) =>
          (String, String) -> Symbolic (SBV a, SBV b)
sTuple t = t & _1 %%~ exists >>= _2 %%~ exists

-- traverseTuple :: Monad m => (forall x. f x -> m (g x)) ->
--                  (f a, f b) -> m (g a, g b)
-- traverseTuple f t = t & _1 %%~ f >>= _2 %%~ f

traverseTuple :: Monad m => (f a -> m (g a)) -> (f b -> m (g b)) ->
                 (f a, f b) -> m (g a, g b)
traverseTuple f f' t = t & _1 %%~ f >>= _2 %%~ f'

sTuple' :: forall a b. (SymWord a, SymWord b) =>
          (String, String) -> Symbolic (SBV a, SBV b)
sTuple' (a, b) = (Constant a, Constant b) & traverseTuple f f where
  f :: forall x. SymWord x => Constant String x -> Symbolic (SBV x)
  f = exists . getConstant
-}
