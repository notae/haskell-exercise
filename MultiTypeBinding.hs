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

class Monad (m s) => Binding m s v where
  type family Var m s v :: *
  newVar    :: v         -> (m s) (Var m s v)
  lookupVar :: Var m s v -> (m s) v
  updateVar :: Var m s v -> v -> (m s) ()

-- An implementation of binding with State monad + IntMap

type IMVarId = IntMap.Key

data IMVar s v = IMVar { imVarId :: IMVarId } deriving (Show, Eq)

class IMEnvId e where
  getId :: e -> IMVarId
  setId :: e -> IMVarId -> e

class IMEnvMap e v where
  getMap :: e -> IntMap v
  setMap :: e -> IntMap v -> e

newtype IM e s a =
  IM { unIM :: State e a }
  deriving (Functor, Applicative, Monad, MonadState e)

instance (IMEnvId e , IMEnvMap e v) => Binding (IM e) s v where
  type Var (IM e) s v = IMVar s v
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

runIM :: (forall s. IM e s a) -> e -> (a, e)
runIM im initialEnv = runState (unIM im) initialEnv

evalIM :: (forall s. IM e s a) -> e -> a
evalIM im initialEnv = evalState (unIM im) initialEnv

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

runMyIM0 :: (forall s. IM MyIMEnv s a) -> (a, MyIMEnv)
runMyIM0 im = runIM im initialMyIMEnv

evalMyIM0 :: (forall s. IM MyIMEnv s a) -> a
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
>>> testIM
(((True,Red),(False,Blue)),MyIMEnv {imNextId = 2, imIMap = fromList [], imBMap = fromList [(0,False)], imCMap = fromList [(1,Blue)]})
-}
testIM :: (((Bool, Color), (Bool, Color)), MyIMEnv)
testIM = runIM prog initialMyIMEnv

--   Type check with phantom type parameter

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
