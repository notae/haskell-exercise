{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module CP where

import           Control.Applicative  (Alternative, Applicative, (<$>), (<*>))
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Set             as S
import           Debug.Trace          (traceShow, traceShowM)

import qualified Data.Graph.Inductive.Internal.Queue as Q

import           Control.Lens
import qualified Data.Fuzzy   as F

--
-- CP(FD) with Lens/Prism
--

newtype FD v a =
  FD { unFD :: StateT (FDState v) [] a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

data FDState v =
  FDState
  { _pcons :: [FD v Grade]
  , _stack :: [v]
  , _vars  :: v
  }

initState :: v -> FDState v
initState v0 = FDState [] [] v0

type Var v a = Lens' v (Dom a)

type Grade = F.RGrade
type Dom v = F.FS v Grade
type Val v = F.FValue v

makeLenses ''FDState

runFD :: v -> FD v a -> [a]
runFD v0 dsl = evalStateT `flip` initState v0 $ unFD dsl

useV :: Var v a -> FD v (Dom a)
useV l = FD $ use (vars . l)

useS :: Val a => Var v a -> FD v (Maybe a)
useS l = do
  as <- useV l
  return $ if F.size as <= 1 then listToMaybe (F.support as) else Nothing

setS :: Val a => Var v a -> a -> FD v ()
setS l a = FD $ vars . l .= F.fromCoreList [a]

push :: FD v ()
push = do
  v <- FD $ use vars
  FD $ stack %= (v:)

pop :: FD v ()
pop = do
  vs <- FD $ use stack
  FD $ vars .= head vs
  FD $ stack .= tail vs

addPCons :: FD v Grade -> FD v ()
addPCons c = FD $ pcons %= (c:)

cons2 :: (Val a, Val b, Show v) =>
         Var v a -> Var v b -> (a -> b -> Grade) -> FD v Grade
cons2 a b p = do
  ma <- useS a
  mb <- useS b
  let g = p <$> ma <*> mb
  v' <- FD $ use vars
  traceShowM (ma, mb, g, v')
  return $ fromMaybe maxBound g

eval :: FD v Grade
eval = do
  es <- FD $ use pcons
  liftM F.fand $ sequence es

label :: Val a => Var v a -> FD v a
label l = do
  as <- useV l
  a <- FD $ lift (F.support as)
  setS l a
  -- TBD: propagate arc.cons
  return a

--
-- User code
--

type V = (Int, Bool)
type Vs = (Dom Int, Dom Bool)

cp1 :: FD Vs (V, Grade)
cp1 = do
  -- add primitive constraints
  addPCons $ cons2 _1 _2 pred2

  -- TBD: map domains
--   vs <- FD $ use vars

  -- TBD: map lenses
  let vs = (_1, _2) :: (Var Vs Int, Var Vs Bool)
  v <- vs & mapMOf _1 label >>= mapMOf _2 label

  -- TBD: pmap [
--   y <- label _2
--   v' <- FD $ use vars
--   g' <- eval
--   traceShowM (v', g')
--   x <- label _1
--   let v = (x, y)
  -- TBD: ] pmap

  -- TBD: eval on local instead of StateT?
  g <- eval
  -- guard $ g > minBound
  return (v, g)

pred2 :: Int -> Bool -> Grade
pred2 x y = g' where
  g = case x `mod` 3 of
       0 -> 1
       1 -> 0.7
       _ -> 0
  g' = if y then g else F.fnot g

test1 :: [(V, Grade)]
test1 = runFD (F.fromCoreList [0..2], F.fromCoreList [True, False]) cp1

--
-- Variables
--

class Vars v where
  expand1  :: v -> (Var v a, [a])
--  lenses ::

vs' :: (Var Vs Int, Var Vs Bool)
vs' = (_1, _2)

{-
ls :: (Lens' Vs [Int], Lens' Vs [Bool])
ls = l2 where
  -- l = (_1, _2)
  l0 :: ((), ())
  l0 = ((), ())
  l1 :: (Lens' Vs [Int], ())
  l1 = l0 & _1 .~ _1
  l2 :: (Lens' Vs [Int], Lens' Vs [Bool])
  l2 = l1 & _2 .~ _2
-}

-- instance Vars (a, b) where
--   vmap vs = vs & mapMOf _1 label >>= mapMOf _2 label


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

--
-- State in search
--

testS :: [((Int, Int), Int)]
testS = runStateT `flip` 1 $ do
  a <- lift [10, 20, 30]
  b <- lift [1, 2, 3]
  modify (*a)
  modify (+b)
  return (a, b)


testW :: (((), [String]), [String])
testW = runWriter $ do
  tell ["start"]
  (a, w) <- listen $ tell ["foo"]
  tell ["end"]
  return (a, w)
