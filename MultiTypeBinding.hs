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
import Data.Set (Set)
import qualified Data.Set as Set

-- Interface for variable binding

class Monad m => Binding m v where
  type family Var m v :: *
  newVar    :: v       -> m (Var m v)
  lookupVar :: Var m v -> m v
  updateVar :: Var m v -> v -> m ()

-- An implementation of binding with State monad + IntMap

type IMVarId = IntMap.Key

data IMVar v = IMVar { imVarId :: IMVarId } deriving (Show, Eq)

data IMEnv =
  IMEnv
  { imNextId :: IMVarId
  , imIMap :: IntMap Int
  , imBMap :: IntMap Bool }
  deriving (Show)

initIMEnv :: IMEnv
initIMEnv =
  IMEnv
  { imNextId = 0
  , imIMap    = IntMap.empty
  , imBMap    = IntMap.empty }

class MonadState IMEnv m => IMBinding m v where
  getMap :: m (IntMap v)
  setMap :: IntMap v -> m ()

instance IMBinding m v => Binding m v where
  type Var m v = IMVar v
  newVar v = do
    vid <- gets imNextId
    modify $ \s -> s { imNextId = vid + 1 }
    let var = IMVar vid
    updateVar var v
    return var
  lookupVar (IMVar vid) = do
    si <- getMap
    return $ si IntMap.! vid
  updateVar (IMVar vid) v = do
    si <- getMap
    setMap $ IntMap.insert vid v si

-- Declaration per type

instance MonadState IMEnv m => IMBinding m Int where
  getMap = gets imIMap
  setMap m = modify $ \s -> s { imIMap = m }

instance MonadState IMEnv m => IMBinding m Bool where
  getMap = gets imBMap
  setMap m = modify $ \s -> s { imBMap = m }

-- Example for use

progIM :: State IMEnv ((Int, Bool), (Int, Bool))
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
(((123,True),(456,False)),IMEnv {imNextId = 2, imIMap = fromList [(0,456)], imBMap = fromList [(1,False)]})
-}
testIM :: (((Int, Bool), (Int, Bool)), IMEnv)
testIM = runState progIM initIMEnv

progIM2New :: State IMEnv (IMVar Int)
progIM2New = newVar 123

progIM2Use :: IMVar Int -> State IMEnv Int
progIM2Use v = lookupVar v

-- Should causes type error
testIM2 :: (Int, IMEnv)
testIM2 = runState (progIM2Use (evalState progIM2New initIMEnv)) initIMEnv
