-- Example of Multiple Type Binding

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative (Alternative)
import Control.Applicative (Applicative)
import Control.Monad (MonadPlus)
import Control.Monad.ST (ST)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState)
import Control.Monad.State (StateT)
import Control.Monad.State (evalStateT)
import Control.Monad.State (gets)
import Control.Monad.State (modify)
import Control.Monad.State (runStateT)
import Data.Functor.Identity (runIdentity)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.STRef (STRef)
import Data.STRef (newSTRef)
import Data.STRef (readSTRef)
import Data.STRef (writeSTRef)
import Control.Monad.Trans (lift)

-- Interface for variable binding

class Monad (m s) => Binding m s v where
  type Var m s v :: *
  newVar    :: v         -> (m s) (Var m s v)
  lookupVar :: Var m s v -> (m s) v
  updateVar :: Var m s v -> v -> (m s) ()

-- An implementation of binding with ST monads

instance Binding ST s v where
  type Var ST s v = STRef s v
  newVar = newSTRef
  lookupVar = readSTRef
  updateVar = writeSTRef

-- An implementation of binding with State monad + IntMap

type IMVarId = IntMap.Key

data IMVar s v = IMVar { imVarId :: IMVarId } deriving (Show, Eq)

class IMEnvId e where
  getId :: e -> IMVarId
  setId :: e -> IMVarId -> e

class IMEnvMap e v where
  getMap :: e -> IntMap v
  setMap :: e -> IntMap v -> e

newtype IM e m s a =
  IM { unIM :: StateT e m a }
  deriving (Functor, Applicative, Alternative,
            Monad, MonadPlus, MonadState e)

instance (Monad m, IMEnvId e , IMEnvMap e v) => Binding (IM e m) s v where
  type Var (IM e m) s v = IMVar s v
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

runIM :: (forall s. IM e m s a) -> e -> m (a, e)
runIM im = runStateT (unIM im)

evalIM :: Monad m => (forall s. IM e m s a) -> e -> m a
evalIM im = evalStateT (unIM im)

--   Declaration per type (with aconcrete types)

data MyIMEnv =
  MyIMEnv
  { imNextId :: IMVarId
  , imIMap   :: IntMap Int
  , imBMap   :: IntMap Bool
  , imCMap   :: IntMap Color
  , imILMap  :: IntMap [Int]
  , imBLMap  :: IntMap [Bool]
  , imCLMap  :: IntMap [Color] }
  deriving (Show)

initialMyIMEnv :: MyIMEnv
initialMyIMEnv =
  MyIMEnv
  { imNextId = 0
  , imIMap   = IntMap.empty
  , imBMap   = IntMap.empty
  , imCMap   = IntMap.empty
  , imILMap   = IntMap.empty
  , imBLMap   = IntMap.empty
  , imCLMap   = IntMap.empty }

runMyIM0 :: (forall s. IM MyIMEnv m s a) -> m (a, MyIMEnv)
runMyIM0 im = runIM im initialMyIMEnv

evalMyIM0 :: Monad m => (forall s. IM MyIMEnv m s a) -> m a
evalMyIM0 im = evalIM im initialMyIMEnv

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

instance IMEnvMap MyIMEnv [Int] where
  getMap = imILMap
  setMap e m = e { imILMap = m }

instance IMEnvMap MyIMEnv [Bool] where
  getMap = imBLMap
  setMap e m = e { imBLMap = m }

instance IMEnvMap MyIMEnv [Color] where
  getMap = imCLMap
  setMap e m = e { imCLMap = m }

--   Declaration per type (with 2 type parameters)

data MyIM2Env v1 v2 =
  MyIM2Env
  { im2NextId :: IMVarId
  , im2Map1   :: IntMap v1
  , im2Map2   :: IntMap v2 }
  deriving (Show)

initialMyIM2Env :: MyIM2Env v1 v2
initialMyIM2Env =
  MyIM2Env
  { im2NextId = 0
  , im2Map1   = IntMap.empty
  , im2Map2   = IntMap.empty }

runMyIM20 :: (forall s. IM (MyIM2Env v1 v2) m s a) -> m (a, MyIM2Env v1 v2)
runMyIM20 im = runIM im initialMyIM2Env

evalMyIM20 :: Monad m => (forall s. IM (MyIM2Env v1 v2) m s a) -> m a
evalMyIM20 im = evalIM im initialMyIM2Env

instance IMEnvId (MyIM2Env v1 v2) where
  getId = im2NextId
  setId e vid = e { im2NextId = vid }

instance IMEnvMap (MyIM2Env v1 v2) v1 where
  getMap = im2Map1
  setMap e m = e { im2Map1 = m }

instance IMEnvMap (MyIM2Env v1 v2) v2 where
  getMap = im2Map2
  setMap e m = e { im2Map2 = m }

-- Example for use

prog :: (Binding m s Bool, Binding m s Color) =>
        m s ((Bool, Color), (Bool, Color))
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
>>> testST
((True,Red),(False,Blue))
-}
testST :: ((Bool, Color), (Bool, Color))
testST = runST prog

{-|
>>> testIM
(((True,Red),(False,Blue)),MyIMEnv {imNextId = 2, imIMap = fromList [], imBMap = fromList [(0,False)], imCMap = fromList [(1,Blue)]})
-}
testIM :: (((Bool, Color), (Bool, Color)), MyIMEnv)
testIM = runIdentity $ runMyIM0 prog

-- with List monad as inner monad
{-|
>>> testIML
[(True,Red),(True,Green),(True,Blue),(False,Red),(False,Green),(False,Blue)]
-}
testIML :: [(Bool, Color)]
testIML = evalMyIM0 $ do
  vb <- newVar [True, False]
  vc <- newVar [Red, Green, Blue]
  lb <- lookupVar vb
  lc <- lookupVar vc
  b <- IM $ lift lb
  c <- IM $ lift lc
  return (b, c)

-- with predefined instances of class IMEnvId, IMEnvMap

progIM2 :: (Binding m e Bool, Binding m e Color) => m e (Bool, Color)
progIM2 = do
  vb <- newVar True
  vc <- newVar Green
  b <- lookupVar vb
  c <- lookupVar vc
  return (b, c)

{-|
>>> testIM2
((True,Green),MyIM2Env {im2NextId = 2, im2Map1 = fromList [(0,True)], im2Map2 = fromList [(1,Green)]})
-}
testIM2 :: ((Bool, Color), MyIM2Env Bool Color)
testIM2 = runIdentity $ runMyIM20 progIM2

-- Type check with phantom type parameter

--     Exporting variable should cause type error

-- progIMExportError :: (Binding m s Int) => m s (Var m s Int)
-- progIMExportError = newVar (123::Int)

-- progIMExportError :: IM MyIMEnv s (Var (IM MyIMEnv) s Int)
-- progIMExportError = newVar 123

-- testIMExportError = runMyIM0 (newVar 123)

--     Importing variable should cause type error

-- progIMImportError :: (Binding m s Int) => Var m s Int -> m s Int
-- progIMImportError v = lookupVar v

-- progIMImportError :: Var (IM MyIMEnv) s Int -> IM MyIMEnv s Int
-- progIMImportError v = lookupVar v

-- testIMImportError :: Var (IM MyIMEnv) s Int -> (Int, MyIMEnv)
-- testIMImportError v = runMyIM0 (lookupVar v)
