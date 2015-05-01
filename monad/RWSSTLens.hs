{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module RWSSTLens where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.ST.Lazy
import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import qualified Data.Sequence         as Seq
import           Data.STRef.Lazy

import Control.Lens

--
-- newtype with Lens
--

--
-- Library code
--

class Monad m => DSL m where
  type Ref m
  putLog :: String -> m ()
  readVar :: String -> m (Maybe Int)
  newRef :: Int -> m (Ref m)
  getRef :: m (Maybe (Ref m))
  readRef :: Ref m -> m Int
  modifyRef :: Ref m -> (Int -> Int) -> m ()
  localRef :: Ref m -> m Int -> m Int

newtype DSL1 s a =
  DSL1 { unDSL1 :: RWST (Env1 s) Log DSL1State (ST s) a }
  deriving (Functor, Applicative, Monad)

data Env1 s =
  Env1
  { _traceFlag :: Bool
  , _binding   :: Map.Map String Int
  , _var       :: Maybe (Ref (DSL1 s))
  }

type Log = Seq.Seq String

data DSL1State =
  DSL1State
  { _count :: Int
  } deriving (Show)

makeLenses ''Env1
makeLenses ''DSL1State

runDSL1 :: Bool -> (forall s. DSL1 s a) -> (a, DSL1State, Log)
runDSL1 tf m = runST $ runRWST (unDSL1 m) (initEnv1 tf) initState1

initEnv1 :: Bool -> Env1 s
initEnv1 tf =
  Env1
  { _traceFlag = tf
  , _binding = Map.fromList [("one", 1), ("two", 2)]
  , _var = Nothing
  }

initState1 :: DSL1State
initState1 = DSL1State { _count = 0 }

instance DSL (DSL1 s) where
  type Ref (DSL1 s) = STRef s Int
  putLog l = do
    f <- view1 traceFlag
    DSL1 $ when f $ tell $ Seq.singleton l
  readVar n = view1 (binding . at n)
  newRef i = liftST $ newSTRef i
  getRef = view1 var
  readRef r = liftST $ readSTRef r
  modifyRef r f = liftST $ modifySTRef r f
  localRef r m = DSL1 $ local (var .~ Just r) (unDSL1 m)

liftST :: ST s a -> DSL1 s a
liftST = DSL1 . lift

view1 :: Getting a (Env1 s) a -> DSL1 s a
view1 = DSL1 . view

--
-- User code
--

{-|
>>> test
(1,DSL1State {_count = 0},fromList ["start","val:1","var:1","i3:200","end"])
-}
test :: (Int, DSL1State, Log)
test = runDSL1 True testDSL1

{-|
>>> testWithoutLog
(1,DSL1State {_count = 0},fromList [])
-}
testWithoutLog :: (Int, DSL1State, Log)
testWithoutLog = runDSL1 False testDSL1

testDSL1 :: DSL m => m Int
testDSL1 = do
  putLog "start"
  i <- readVar "one"
  let i' = fromMaybe 999 i
  putLog $ "val:" ++ show i'
  v <- newRef i'
  i2 <- localRef v testDSLLocal
  putLog $ "var:" ++ show i2
  let ?ref = v
  i3 <- testDSLSub
  putLog $ "i3:" ++ show i3
  putLog "end"
  return i2

testDSLLocal :: DSL m => m Int
testDSLLocal = do
  Just v2 <- getRef
  readRef v2

testDSLSub :: (DSL m, ?ref::Ref m) => m Int
testDSLSub = do
  testDSLSub2
  i <- readRef ?ref
  return $ i * 100

testDSLSub2 :: (DSL m, ?ref::Ref m) => m ()
testDSLSub2 = modifyRef ?ref succ
