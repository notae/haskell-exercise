{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lens where

import Control.Applicative (Alternative, Applicative)
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
  , _var   :: Var
  }

type Var = (Int, Int, Int)

makeLenses ''DSLState

($=) :: Lens' Var Int -> Int -> DSL ()
l $= i = DSL $ var . l .= i

dsl :: DSL Var
dsl = do
  _1 $= 2
  _2 $= 3
  _3 $= 5
  DSL $ use var

{-|
>>> testDSL
[(2,3,5)]
-}
testDSL :: [Var]
testDSL = evalStateT `flip` (DSLState [] (0, 0, 0)) $ unDSL $ dsl
