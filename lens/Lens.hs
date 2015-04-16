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


--
-- DSL with Lens/Prism
--

newtype DSL a =
  DSL { unDSL :: StateT (DSLState DSL) [] a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus,
            MonadState (DSLState DSL))

data DSLState m =
  DSLState
  { _exprs :: [m Bool]
  , _vars  :: Vars
  }

type MonadDSL m = (Applicative m, MonadState (DSLState m) m)

type Vars = (Int, Int, Int)
type Var = Lens' Vars Int

makeLenses ''DSLState

runDSL :: DSL a -> [a]
runDSL dsl = evalStateT `flip` (DSLState [] (0, 0, 0)) $ unDSL $ dsl

infixr 0 $=
($=) :: MonadDSL m => Var -> Int -> m ()
l $= i = vars . l .= i

infixl 7 $*
($*) :: MonadDSL m => Var -> Var -> m Int
x $* y = (*) <$> (use (vars . x)) <*> (use (vars . y))

addExpr :: MonadDSL m => m Bool -> m ()
addExpr e = exprs %= (e:)

addPred :: MonadDSL m => (Int -> Int -> Bool) -> Var -> Var -> m ()
addPred p lx ly = addExpr $ do
  x <- use (vars . lx)
  y <- use (vars . ly)
  return $ p x y

{-|
>>> runDSL dsl1
[(2,3,5)]
-}
dsl1 :: MonadDSL m => m Vars
dsl1 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  use vars

{-|
>>> runDSL dsl2
[6]
-}
dsl2 :: MonadDSL m => m Int
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
eval :: MonadDSL m => m Bool
eval = do
  es <- use exprs
  liftM and $ sequence es

dsl3 :: MonadDSL m => m Bool
dsl3 = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  addPred pred1 _1 _2
  addPred pred1 _2 _3
  -- addExpr $ (*) <$> use _1 <*> use _2
  eval

msAction :: Monad m => m Int
msAction = return 1

msAction2 :: MonadDSL m => m Int
msAction2 = use (vars . _1)

dsl4 :: MonadDSL m => m Int
dsl4 = do
  msAction
  msAction2
