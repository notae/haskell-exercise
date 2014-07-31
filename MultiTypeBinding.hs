-- Example of Multiple Type Binding

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.State (MonadState)
import Control.Monad.State (gets)
import Control.Monad.State (modify)
import Control.Monad.State (State)
import Control.Monad.State (runState)
import Control.Monad.State (evalState)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

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
  , imIMap :: IntMap Int
  , imBMap :: IntMap Bool }
  deriving (Show)

initMyIMEnv :: MyIMEnv
initMyIMEnv =
  MyIMEnv
  { imNextId = 0
  , imIMap    = IntMap.empty
  , imBMap    = IntMap.empty }

instance IMEnvId MyIMEnv where
  getId = imNextId
  setId e vid = e { imNextId = vid }

instance IMEnvMap MyIMEnv Int where
  getMap = imIMap
  setMap e m = e { imIMap = m }

instance IMEnvMap MyIMEnv Bool where
  getMap = imBMap
  setMap e m = e { imBMap = m }

-- Example for use

progIM :: State MyIMEnv ((Int, Bool), (Int, Bool))
progIM = do
  vi <- newVar 123
  vb <- newVar True
  i1 <- lookupVar vi
  b1 <- lookupVar vb
  updateVar vi 456
  updateVar vb False
  i2 <- lookupVar vi
  b2 <- lookupVar vb
  return ((i1, b1), (i2, b2))

{-|
>>> testIM
(((123,True),(456,False)),MyIMEnv {imNextId = 2, imIMap = fromList [(0,456)], imBMap = fromList [(1,False)]})
-}
testIM :: (((Int, Bool), (Int, Bool)), MyIMEnv)
testIM = runState progIM initMyIMEnv

progIM2New :: State MyIMEnv (IMVar Int)
progIM2New = newVar 123

progIM2Use :: IMVar Int -> State MyIMEnv Int
progIM2Use v = lookupVar v

-- Should causes type error
testIM2 :: (Int, MyIMEnv)
testIM2 = runState (progIM2Use (evalState progIM2New initMyIMEnv)) initMyIMEnv
