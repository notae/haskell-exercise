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
import           Data.STRef.Lazy

import Control.Lens

-- import RWST

--
-- newtype with Lens
--

data Env s =
  Env
  { _traceFlag :: Bool
  , _binding   :: Map.Map String Int
  , _var       :: Maybe (STRef s Int)
  }

makeLenses ''Env

initEnv :: Bool -> Env s
initEnv tf = Env tf (Map.fromList [("one", 1), ("two", 2)]) Nothing

type Log = [String]

data DSLState =
  DSLState
  { _count :: Int
  } deriving (Show)

makeLenses ''DSLState

initState :: DSLState
initState = DSLState { _count = 0 }

newtype DSL s a =
  DSL { unDSL :: RWST (Env s) Log DSLState (ST s) a }
  deriving (Functor, Applicative, Monad)

-- runDSL :: forall s. DSL s a -> Env s -> DSLState -> (a, DSLState, Log)
-- runDSL m r s = runST $ runRWST (unDSL m) r s

test :: Bool -> (Int, DSLState, Log)
test tf = runST $ runRWST (unDSL testRWSST) (initEnv tf) initState

putLog :: String -> DSL s ()
putLog l = do
  f <- DSL $ view traceFlag
  DSL $ when f $ tell [l]

testRWSST :: DSL s Int
testRWSST = do
  putLog "start"
  i <- DSL $ view (binding . at "one")
  let i' = maybe 999 id i
  putLog $ "val:" ++ show i'
  v <- DSL $ lift $ newSTRef i'
  i2 <- DSL $ local (var .~ Just v) $ do
    Just v2 <- asks _var
    lift $ readSTRef v2
  putLog $ "var:" ++ show i2
  putLog $ "end"
  return i2
