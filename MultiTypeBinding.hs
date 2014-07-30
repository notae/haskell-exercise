-- Example of Multiple Type Binding

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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


type VarId = IntMap.Key
newtype Var v = Var { varId :: VarId } deriving (Show, Eq)

data Env =
  Env
  { fdINextId :: VarId
  , fdIMap :: IntMap Int
  , fdBNextId :: VarId
  , fdBMap :: IntMap Bool }
  deriving (Show)

initState :: Env
initState =
  Env
  { fdINextId = 0
  , fdIMap    = IntMap.empty
  , fdBNextId = 0
  , fdBMap    = IntMap.empty }

class MonadState s m => Binding m s v where
  newVar    :: v     -> m (Var v)
  lookupVar :: Var v -> m v
  updateVar :: Var v -> v -> m ()

instance MonadState Env m => Binding m Env Int where
  newVar v = do
    vid <- gets fdINextId
    modify $ \s -> s { fdINextId = vid + 1 }
    let var = Var vid
    updateVar var v
    return var
  lookupVar (Var vid) = do
    si <- gets fdIMap
    return $ si IntMap.! vid
  updateVar (Var vid) v = do
    si <- gets fdIMap
    modify $ \s -> s { fdIMap = IntMap.insert vid v si }

instance MonadState Env m => Binding m Env Bool where
  newVar v = do
    vid <- gets fdBNextId
    modify $ \s -> s { fdBNextId = vid + 1 }
    let var = Var vid
    updateVar var v
    return var
  lookupVar (Var vid) = do
    si <- gets fdBMap
    return $ si IntMap.! vid
  updateVar (Var vid) v = do
    sb <- gets fdBMap
    modify $ \s -> s { fdBMap = IntMap.insert vid v sb }

prog :: State Env ((Int, Bool), (Int, Bool))
prog = do
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
>>> test
(((123,True),(456,False)),Env {fdINextId = 1, fdIMap = fromList [(0,456)], fdBNextId = 1, fdBMap = fromList [(0,False)]})
-}
test :: (((Int, Bool), (Int, Bool)), Env)
test = runState prog initState
