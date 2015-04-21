{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module CP where

import Control.Applicative (Alternative, Applicative, (<$>), (<*>))
import Control.Lens
import Control.Monad.State
import qualified Data.Graph.Inductive.Internal.Queue as Q
import qualified Data.Set as S
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Data.Fuzzy as F

--
-- CP(FD) with Lens/Prism
--

newtype FD v a =
  FD { unFD :: StateT (FDState v) [] a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

data FDState v =
  FDState
  { _pcons :: [FD v Grade]
  , _vars  :: v
  }

type Var v a = Lens' v a

type Grade = F.RGrade
type Domain v = F.FS v Grade
type FDValue v = F.FValue v

makeLenses ''FDState

runFD :: v -> FD v a -> [a]
runFD v0 dsl = evalStateT `flip` FDState [] v0 $ unFD dsl

useV :: Var v a -> FD v a
useV l = FD $ use (vars . l)

useS :: Var v [a] -> FD v (Maybe a)
useS l = useV l >>= return . listToMaybe

setS :: Var v [a] -> a -> FD v ()
setS l a = FD $ vars . l .= [a]

addPCons :: FD v Grade -> FD v ()
addPCons c = FD $ pcons %= (c:)

cons2 :: Var v [a] -> Var v [b] -> (a -> b -> Grade) -> FD v Grade
cons2 a b p = do
  ma <- useS a
  mb <- useS b
  let g = p <$> ma <*> mb
  return $ fromMaybe maxBound g

eval :: FD v Grade
eval = do
  es <- FD $ use pcons
  liftM F.fand $ sequence es

label :: Lens' v [a] -> FD v a
label l = do
  as <- useV l
  a <- FD $ lift as
  setS l a
  -- TBD: propagate arc.cons
  return a

--
-- User code
--

cp1 :: FD ([Int], [Bool]) ((Int, Bool), Grade)
cp1 = do
  addPCons $ cons2 _1 _2 pred2
  -- TBD: pmap [
  y <- label _2
  x <- label _1
  let v = (x, y)
  -- TBD: ] pmap
  -- TBD: eval on local instead of StateT?
  g <- eval
  guard $ g > minBound
  return (v, g)

pred2 :: Int -> Bool -> Grade
pred2 x y = g' where
  g = case x `mod` 3 of
       0 -> 1
       1 -> 0.7
       2 -> 0
  g' = if y then g else F.fnot g

test1 :: [((Int, Bool), Grade)]
test1 = runFD ([0..2],[True,False]) cp1

--
-- lift up/down
--

{-
class LiftUp f where
  liftup :: (a -> t a) -> f a -> f (t a)

instance LiftUp (a, b) where
  liftup f (a, b) = (f a, f b)
-}

class LiftDown f where
  liftdown :: (t a -> a) -> f (t a) -> f a

mapWithLens :: (Field1 s s b b, Field2 s s b b, Field3 s s b b) => s -> [b]
mapWithLens s = map (\l -> s ^. l) [_1,_2,_3]
