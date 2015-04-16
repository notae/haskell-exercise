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


--
-- DSL with Lens/Prism
--

newtype DSL a =
  DSL { unDSL :: StateT DSLState [] a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

data DSLState =
  DSLState
  { _exprs :: [DSL Bool]
  , _vars  :: Vars
  }

type Vars = (Int, Int, Int)
type Var = Lens' Vars Int

makeLenses ''DSLState

infixr 0 $=
($=) :: Var -> Int -> DSL ()
l $= i = DSL $ vars . l .= i

infixl 7 $*
($*) :: Var -> Var -> DSL Int
x $* y = (*) <$> (DSL $ use (vars . x)) <*> (DSL $ use (vars . y))

addExpr :: DSL Bool -> DSL ()
addExpr e = DSL $ exprs %= (e:)

addPred :: (Int -> Int -> Bool) -> Var -> Var -> DSL ()
addPred p lx ly = addExpr $ do
  x <- DSL $ use (vars . lx)
  y <- DSL $ use (vars . ly)
  return $ p x y

{-|
>>> runDSL dsl1
[(2,3,5)]
-}
dsl1 :: DSL Vars
dsl1 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  DSL $ use vars

{-|
>>> runDSL dsl2
[6]
-}
dsl2 :: DSL Int
dsl2 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  n <- _1 $* _2
  return n

-- an user defined predicate
pred1 :: Int -> Int -> Bool
pred1 x y = abs (x - y) < 2

{-|
>>> runDSL dsl3
[False]
-}
eval :: DSL Bool
eval = do
  es <- DSL $ use exprs
  liftM and $ sequence es

dsl3 :: DSL Bool
dsl3 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  addPred pred1 _1 _2
  addPred pred1 _2 _3
  -- addExpr $ (*) <$> use _1 <*> use _2
  eval

runDSL :: DSL a -> [a]
runDSL dsl = evalStateT `flip` (DSLState [] (0, 0, 0)) $ unDSL $ dsl
