-- Example of Multiple Type Binding

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad.State (MonadState)
import Control.Monad.State (gets)
import Control.Monad.State (modify)
import Control.Monad.State (State)
import Control.Monad.State (runState)
import Control.Monad.State (evalState)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Applicative (Applicative)

-- Interface for variable binding

class Monad m => Binding m v where
  type family Var m v :: *
  newVar    :: v       -> m (Var m v)
  lookupVar :: Var m v -> m v
  updateVar :: Var m v -> v -> m ()

-- An implementation of binding with State monad + IntMap

type IMVarId = IntMap.Key

data IMVar v = IMVar { imVarId :: IMVarId } deriving (Show, Eq)

class IMEnvId e where
  getId :: e -> IMVarId
  setId :: e -> IMVarId -> e

class IMEnvMap e v where
  getMap :: e -> IntMap v
  setMap :: e -> IntMap v -> e

instance (MonadState s m, IMEnvId s, IMEnvMap s v) => Binding m v where
  type Var m v = IMVar v
  newVar v = do
    vid <- gets getId
    modify $ \s -> setId s (vid + 1)
    let var = IMVar vid
    updateVar var v
    return var
  lookupVar (IMVar vid) = do
    si <- gets getMap
    return $ si IntMap.! vid
  updateVar (IMVar vid) v = do
    si <- gets getMap
    modify $ \s -> setMap s $ IntMap.insert vid v si

--   Declaration per type

data MyIMEnv =
  MyIMEnv
  { imNextId :: IMVarId
  , imIMap   :: IntMap Int
  , imBMap   :: IntMap Bool
  , imCMap   :: IntMap Color }
  deriving (Show)

initialMyIMEnv :: MyIMEnv
initialMyIMEnv =
  MyIMEnv
  { imNextId = 0
  , imIMap   = IntMap.empty
  , imBMap   = IntMap.empty
  , imCMap   = IntMap.empty }

instance IMEnvId MyIMEnv where
  getId = imNextId
  setId e vid = e { imNextId = vid }

instance IMEnvMap MyIMEnv Int where
  getMap = imIMap
  setMap e m = e { imIMap = m }

instance IMEnvMap MyIMEnv Bool where
  getMap = imBMap
  setMap e m = e { imBMap = m }

data Color = Red | Green | Blue deriving (Show)

instance IMEnvMap MyIMEnv Color where
  getMap = imCMap
  setMap e m = e { imCMap = m }

newtype IM s e a =
  IM { unIM :: State e a }
  deriving (Functor, Applicative, Monad, MonadState e)

runIM :: (forall s. IM s e a) -> e -> (a, e)
runIM im initialEnv = runState (unIM im) initialEnv

evalIM :: (forall s. IM s e a) -> e -> a
evalIM im initialEnv = evalState (unIM im) initialEnv

-- Example for use

prog :: (Binding m Bool, Binding m Color) => m ((Bool, Color), (Bool, Color))
prog = do
  vb <- newVar True
  vc <- newVar Red
  b <- lookupVar vb
  c <- lookupVar vc
  updateVar vb False
  updateVar vc Blue
  b' <- lookupVar vb
  c' <- lookupVar vc
  return ((b, c), (b', c'))

{-|
>>> testIM
(((True,Red),(False,Blue)),MyIMEnv {imNextId = 2, imIMap = fromList [], imBMap = fromList [(0,False)], imCMap = fromList [(1,Blue)]})
-}
testIM :: (((Bool, Color), (Bool, Color)), MyIMEnv)
testIM = runIM prog initialMyIMEnv

--   with phantom type parameter

progIM2New :: (Binding m Int) => m (Var m Int)
progIM2New = newVar 123

progIM2Use :: (Binding m Int) => Var m Int -> m Int
progIM2Use v = lookupVar v

--     Should causes type error
testIM2 :: (Int, MyIMEnv)
testIM2 = runIM (progIM2Use (evalIM progIM2New initialMyIMEnv)) initialMyIMEnv
