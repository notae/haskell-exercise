{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lens where

import Control.Applicative (Alternative, Applicative, (<$>), (<*>))
import Control.Lens
import Control.Monad.State

type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup =
  Meetup { _name :: String, _location :: (Latitude, Longitude) }
  deriving (Show, Eq)
makeLenses ''Meetup

{-|
with State monad
>>> testState
(True,Meetup {_name = "foo", _location = (2.0,4.0)})
-}

testState :: (Bool, Meetup)
testState = runState `flip` Meetup "foo" (2, 3) $ do
  location . _2 %= (+1)
  lo <- use (location . _2)
  return $ lo > 1

{-|
>>> [(1,"one"),(2,"two")] ^.. traverse . _2
["one","two"]
-}

testRef :: Lens' Meetup Degrees -> Meetup -> Bool
testRef l s = s ^. l > 2

{-|
>>> runIdentity $ mapMOf traverse (\i -> Identity $ take i $ repeat '*') [1,2,3] :: [String]
["*","**","***"]
-}

-- Traversal
{-|
>>> mapMOf (traverse._2) (\xs -> putStrLn (reverse xs)) [(12,"foo"),(34,"bar")]
oof
rab
[(12,()),(34,())]
>>> mapMOf (both._2) (\xs -> putStrLn (reverse xs)) ((12,"foo"),(34,"bar"))
oof
rab
((12,()),(34,()))
>>> (fromMaybe [] $ [(123,True),(456,False)] & mapMOf (each._1) (\a -> Just (a:[]))) & mapMOf (each._2) (\a -> Just (a:[]))
Just [([123],[True]),([456],[False])]
-}

--
-- DSL with Lens/Prism
--

newtype DSL v a =
  DSL { unDSL :: StateT (DSLState v) [] a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

data DSLState v =
  DSLState
  { _exprs :: [DSL v Bool]
  , _vars  :: v
  }

type Var v a = Lens' v a

makeLenses ''DSLState

runDSL :: v -> DSL v a -> [a]
runDSL v0 dsl = evalStateT `flip` DSLState [] v0 $ unDSL dsl

infixr 0 $=
($=) :: Var v a -> a -> DSL v ()
l $= i = DSL $ vars . l .= i

infixr 0 $@
($@) :: Var v a -> [a] -> DSL v ()
l $@ i = (DSL $ lift i) >>= (l $=)

infixl 7 $*
($*) :: Num a => Var v a -> Var v a -> DSL v a
x $* y = DSL $ (*) <$> use (vars . x) <*> use (vars . y)

addExpr :: DSL v Bool -> DSL v ()
addExpr e = DSL $ exprs %= (e:)

addPred :: (a -> b -> Bool) -> Var v a -> Var v b -> DSL v ()
addPred p lx ly = addExpr $ DSL $ p <$> use (vars . lx) <*> use (vars . ly)

eval :: DSL v Bool
eval = do
  es <- DSL $ use exprs
  liftM and $ sequence es

{-|
>>> runDSL (0,0,0) dsl1
[(2,3,5)]
-}
dsl1 :: DSL (Int, Int, Int) (Int, Int, Int)
dsl1 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  DSL $ use vars

{-|
>>> runDSL (0,0,0) dsl2
[6]
-}
dsl2 :: DSL (Int, Int, Int) Int
dsl2 = do
  _1 $= 2
  _2 $= 3
  _1 $* _2

-- an user defined predicate
pred1 :: Int -> Int -> Bool
pred1 x y = abs (x - y) < 2

{-|
>>> runDSL (0,0,0) dsl3
[True,False]
-}
dsl3 :: DSL (Int, Int, Int) Bool
dsl3 = do
  _1 $= 2
  _2 $= 3
  _3 $@ [4, 5]
  addPred pred1 _1 _2
  addPred pred1 _2 _3
  eval
