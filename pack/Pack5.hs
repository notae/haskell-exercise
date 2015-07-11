{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Pack5 where

import Control.Applicative
import Control.Lens
import Control.Monad.ST.Lazy
import Control.Monad.State
import Data.Maybe
import Data.STRef.Lazy

-- Interface
{-
forall a.
  a    a
  f    g
  s -> t -> b
       m
-}

type GNT s t f g =
  forall m. Applicative m => (forall a. f a -> m (g a)) -> s -> m t
type GUnlift t b g = Applicative g => t -> g b
type GUnlift' s b f g =
  (Applicative m, Applicative g) => (forall a. f a -> m (g a)) -> s -> m (g b)

class HasGNT s t f g where
  gnt :: GNT s t f g

class HasUnlift t b g where
  gunlift :: GUnlift t b g

class HasUnlift' s b f g where
  gunlift' :: GUnlift' s b f g

-- Instances
ntTuple :: GNT (f a, f b) (g a, g b) f g
ntTuple f (a, b) = (,) <$> f a <*> f b

instance HasGNT (f a, f b) (g a, g b) f g where
  gnt = ntTuple

unliftTuple :: GUnlift (g a, g b) (a, b) g
unliftTuple (a, b) = (,) <$> a <*> b

instance HasUnlift (g a, g b) (a, b) g where
  gunlift = unliftTuple

unliftTuple' :: GUnlift' (f a, f b) (a, b) f g
unliftTuple' f = liftA unliftTuple . ntTuple f

instance HasUnlift' (f a, f b) (a, b) f g where
  gunlift' = unliftTuple'

testUnliftTuple' :: Maybe [(Int, Bool)]
testUnliftTuple' = gunlift' (Just . maybeToList) (Just (1::Int), Just True)

-- Usecase

data Var s a = Var { getVar :: STRef s [a] }

--   using type synonyms

newV :: GNT t' t [] (Var s) -> t' -> ST s t
newV nt vs = nt (fmap Var . newSTRef) vs

getV :: GNT t' t (Var s) [] -> t' -> ST s t
getV nt vs = nt (readSTRef . getVar) vs

testST :: ST s [(Int, Bool)]
testST = do
  v <- newV ntTuple ([1, 2], [True, False])
  modifySTRef (v^._1 & getVar) (fmap (+1))
  vs <- getV ntTuple v
  return $ unliftTuple vs

--   using type classes

newV' :: HasGNT t' t [] (Var s) => t' -> ST s t
newV' vs = gnt (fmap Var . newSTRef) vs

getV' :: HasGNT t' t (Var s) [] => t' -> ST s t
getV' vs = gnt (readSTRef . getVar) vs

getV'' :: HasUnlift' t' b (Var s) [] => t' -> ST s [b]
getV'' = gunlift' (readSTRef . getVar)

testST' :: ST s [(Int, Bool)]
testST' = do
  v <- newV' ([1::Int, 2], [True, False]) :: ST s (Var s Int, Var s Bool)
  modifySTRef (v^._1 & getVar) (fmap (+1))
  -- modifySTRef (getVar $ fst v) (fmap (+(1::Int)))
  -- modifySTRef (getVar $ snd v) (fmap not)
  -- vs <- getV' v -- :: ST s ([Int], [Bool])
  -- return $ gunlift (vs :: ([Int], [Bool]))
  getV'' v

testState :: MonadState Int m => m Int
testState = do
  return 1
