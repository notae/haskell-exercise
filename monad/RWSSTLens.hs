{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module RWSSTLens where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.ST.Lazy
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Data.STRef.Lazy

import Control.Lens

--
-- newtype with Lens
--

--
-- Library
--

newtype DSL s a =
  DSL { unDSL :: RWST (Env s) Log DSLState (ST s) a }
  deriving (Functor, Applicative, Monad)

data Env s =
  Env
  { _traceFlag :: Bool
  , _binding   :: Map.Map String Int
  , _var       :: Maybe (STRef s Int)
  }

type Log = [String]

data DSLState =
  DSLState
  { _count :: Int
  } deriving (Show)

makeLenses ''Env
makeLenses ''DSLState

runDSL :: Bool -> (forall s. DSL s a) -> (a, DSLState, Log)
runDSL tf m = runST $ runRWST (unDSL m) (initEnv tf) initState

initEnv :: Bool -> Env s
initEnv tf =
  Env
  { _traceFlag = tf
  , _binding = Map.fromList [("one", 1), ("two", 2)]
  , _var = Nothing
  }

initState :: DSLState
initState = DSLState { _count = 0 }

putLog :: String -> DSL s ()
putLog l = do
  f <- DSL $ view traceFlag
  DSL $ when f $ tell [l]

--
-- User code
--

{-|
>>> test
(1,DSLState {_count = 0},["start","val:1","var:1","end"])
-}
test :: (Int, DSLState, Log)
test = runDSL True testDSL

{-|
>>> testWithoutLog
(1,DSLState {_count = 0},[])
-}
testWithoutLog :: (Int, DSLState, Log)
testWithoutLog = runDSL False testDSL

testDSL :: DSL s Int
testDSL = do
  putLog "start"
  i <- DSL $ view (binding . at "one")
  let i' = fromMaybe 999 i
  putLog $ "val:" ++ show i'
  v <- DSL $ lift $ newSTRef i'
  i2 <- DSL $ local (var .~ Just v) $ do
    Just v2 <- asks _var
    lift $ readSTRef v2
  putLog $ "var:" ++ show i2
  putLog $ "end"
  return i2
