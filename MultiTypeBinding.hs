-- Example of Multiple Type Binding

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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


class MonadState s m => Binding m s v where
  type family Var m s v :: *
  newVar    :: v         -> m (Var m s v)
  lookupVar :: Var m s v -> m v
  updateVar :: Var m s v -> v -> m ()


data IMEnv =
  IMEnv
  { fdINextId :: IMVarId
  , fdIMap :: IntMap Int
  , fdBNextId :: IMVarId
  , fdBMap :: IntMap Bool }
  deriving (Show)

data IMVar v = IMVar { imVarId :: IMVarId } deriving (Show, Eq)

initState :: IMEnv
initState =
  IMEnv
  { fdINextId = 0
  , fdIMap    = IntMap.empty
  , fdBNextId = 0
  , fdBMap    = IntMap.empty }

type IMVarId = IntMap.Key

instance MonadState IMEnv m => Binding m IMEnv Int where
  type Var m IMEnv Int = IMVar Int
  newVar v = do
    vid <- gets fdINextId
    modify $ \s -> s { fdINextId = vid + 1 }
    let var = IMVar vid
    updateVar var v
    return var
  lookupVar (IMVar vid) = do
    si <- gets fdIMap
    return $ si IntMap.! vid
  updateVar (IMVar vid) v = do
    si <- gets fdIMap
    modify $ \s -> s { fdIMap = IntMap.insert vid v si }

instance MonadState IMEnv m => Binding m IMEnv Bool where
  type Var m IMEnv Bool = IMVar Bool
  newVar v = do
    vid <- gets fdBNextId
    modify $ \s -> s { fdBNextId = vid + 1 }
    let var = IMVar vid
    updateVar var v
    return var
  lookupVar (IMVar vid) = do
    si <- gets fdBMap
    return $ si IntMap.! vid
  updateVar (IMVar vid) v = do
    sb <- gets fdBMap
    modify $ \s -> s { fdBMap = IntMap.insert vid v sb }

prog :: State IMEnv ((Int, Bool), (Int, Bool))
prog = do
  vi <- newVar (123::Int)
  vb <- newVar True
  i1 <- lookupVar vi
  b1 <- lookupVar vb
  updateVar vi (456::Int)
  updateVar vb False
  i2 <- lookupVar vi
  b2 <- lookupVar vb
  return ((i1, b1), (i2, b2))

{-|
>>> test
(((123,True),(456,False)),IMEnv {fdINextId = 1, fdIMap = fromList [(0,456)], fdBNextId = 1, fdBMap = fromList [(0,False)]})
-}
test :: (((Int, Bool), (Int, Bool)), IMEnv)
test = runState prog initState
