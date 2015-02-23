{-|
Module      : CFPFD2
Description : Constraint Functional Programming over Multiple Finite Domain
Copyright   : (c) notae@me.com, 2014
License     : BSD-style
Maintainer  : notae@me.com
Stability   : experimental
Portability : POSIX

This module provides interfaces for constraint programming
over multiple finite domain in Haskell.
Originally from: <http://overtond.blogspot.jp/2008/07/pre.html>

CFPFD2: CFPFD1 + Multiple Type Binding
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module CFPFD2
       (
--        -- * Binding
--          IM (..)
--          Var
       -- * Monads
         FDBinding
       , FDStat (..)
       , FD
       , evalFD
       , runFD
       , FD2
       , evalFD2
       , runFD2
       -- * Variables
       , FDVar
       , Container (..)
       , newFDVar
       , newFDVars
       , newFDVarT
--       , newFDVarC
       -- * Constraint Store
       -- (for defining custom constraints)
       , ArcPropagator
       , arcConstraint
       , MultiPropagator
       , multiConstraint
       -- * Labelling
       , label
       , labelling
       , labellingT
--       , labellingC
       -- * Primitive Constraint
       -- ** Core Constraint
       , alldiff
       , alldiffF
       -- ** Arithmetic Constraint
       , eq
       , le
       , neq
       , add
       , sub
       , add3
       -- ** Modulo Constraint
       , eqmod
       , neqmod
       , alldiffmod
       )
       where

import Control.Monad (MonadPlus)
import Control.Monad (guard)
import Control.Monad (when)
import Control.Monad (replicateM)
import Control.Monad.State (MonadState)
import Control.Monad.State (StateT)
import Control.Monad.State (evalStateT)
import Control.Monad.State (gets)
import Control.Monad.State (runStateT)
import Control.Monad.State (modify)
import Control.Monad.State (lift)
import Control.Applicative (Applicative)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))
import Control.Applicative (Alternative)
import Control.Applicative (WrappedMonad (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Traversable (traverse)


-- Interface for variable binding

class Monad (m s) => Binding m s v where
  type Var m s :: * -> *
  newVar    :: v         -> m s (Var m s v)
  lookupVar :: Var m s v -> m s v
  updateVar :: Var m s v -> v -> m s ()

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

instance (Monad m, IMEnvId e, IMEnvMap e v) => Binding (IM e m) s v where
  type Var (IM e m) s = IMVar s
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

-- FD monad implementation

data FDStat =
  FDStat
  { countLookupVar  :: Int
  , countUpdateVar  :: Int
  , countUpdateVar' :: Int }
  deriving (Show)

initialFDStat :: FDStat
initialFDStat =
  FDStat
  { countLookupVar  = 0
  , countUpdateVar  = 0
  , countUpdateVar' = 0 }

class FDStatEnv e where
  getFDStat :: e -> FDStat
  setFDStat :: e -> FDStat -> e

class Monad m => FDStatStore m where
  modifyFDStat :: (FDStat -> FDStat) -> m ()

type VarDomain v = Set v

type VarPropagator m s = m s ()

nullPropagator :: Monad (m s) => VarPropagator m s
nullPropagator = return ()

data VarState m s v =
  VarState
  { varDomain     :: VarDomain v
  , varPropagator :: VarPropagator m s }

class (Binding m s (VarState m s v), FDStatStore (m s),
       MonadPlus (m s), Ord v) =>
      FDBinding m s v

instance FDStatEnv e => FDStatStore (IM e [] s) where
  modifyFDStat f = do
    st <- gets getFDStat
    modify $ \s -> setFDStat s (f st)

-- 1 type predefined environment

type FD s v = IM (FDEnv s v) [] s

type FDVar m s v = Var m s (VarState m s v)

data FDEnv s v =
  FDEnv
  { fdStat   :: FDStat
  , fdNextId :: IMVarId
  , fdMap    :: IntMap (VarState (IM (FDEnv s v) []) s v) }

initialFDEnv :: FDEnv s v
initialFDEnv =
  FDEnv
  { fdStat   = initialFDStat
  , fdNextId = 0
  , fdMap    = IntMap.empty }

runFD :: (forall s. FD s v a) -> [(a, FDStat)]
runFD im = map (\(a, s) -> (a, fdStat s)) rs where
  rs = runStateT (unIM im) initialFDEnv

evalFD :: (forall s. FD s v a) -> [a]
evalFD im = evalStateT (unIM im) initialFDEnv

instance IMEnvId (FDEnv s v) where
  getId = fdNextId
  setId e vid = e { fdNextId = vid }

instance IMEnvMap (FDEnv s v) (VarState (IM (FDEnv s v) []) s v) where
  getMap = fdMap
  setMap e m = e { fdMap = m }

instance FDStatEnv (FDEnv s v) where
  getFDStat = fdStat
  setFDStat e st = e { fdStat = st }

instance (IMEnvId (FDEnv s v),
          IMEnvMap (FDEnv s v) (VarState (IM (FDEnv s v) []) s v),
          FDStatEnv (FDEnv s v),
          Monad (IM (FDEnv s v) [] s),
          MonadPlus (IM (FDEnv s v) [] s), Ord v) =>
          FDBinding (IM (FDEnv s v) []) s v where
--          FDBinding (FD v) s v where

-- 2 types predefined environment

data FD2Env s v1 v2 =
  FD2Env
  { fd2Stat   :: FDStat
  , fd2NextId :: IMVarId
  , fd2Map1   :: IntMap (VarState (IM (FD2Env s v1 v2) []) s v1)
  , fd2Map2   :: IntMap (VarState (IM (FD2Env s v1 v2) []) s v2) }

initialFD2Env :: FD2Env s v1 v2
initialFD2Env =
  FD2Env
  { fd2Stat   = initialFDStat
  , fd2NextId = 0
  , fd2Map1   = IntMap.empty
  , fd2Map2   = IntMap.empty }

type FD2 s v1 v2 = IM (FD2Env s v1 v2) [] s

runFD2 :: (forall s. FD2 s v1 v2 a) -> [(a, FDStat)]
runFD2 im = map (\(a, s) -> (a, fd2Stat s)) rs where
  rs = runStateT (unIM im) initialFD2Env

evalFD2 :: (forall s. FD2 s v1 v2 a) -> [a]
evalFD2 im = evalStateT (unIM im) initialFD2Env

instance IMEnvId (FD2Env s v1 v2) where
  getId = fd2NextId
  setId e vid = e { fd2NextId = vid }

instance IMEnvMap (FD2Env s v1 v2) (VarState (IM (FD2Env s v1 v2) []) s v1) where
  getMap = fd2Map1
  setMap e m = e { fd2Map1 = m }

instance IMEnvMap (FD2Env s v1 v2) (VarState (IM (FD2Env s v1 v2) []) s v2) where
  getMap = fd2Map2
  setMap e m = e { fd2Map2 = m }

instance FDStatEnv (FD2Env s v1 v2) where
  getFDStat = fd2Stat
  setFDStat e st = e { fd2Stat = st }

instance (IMEnvId (FD2Env s v1 v2),
          IMEnvMap (FD2Env s v1 v2) (VarState (IM (FD2Env s v1 v2) []) s v),
          FDStatEnv (FD2Env s v1 v2),
          Monad (IM (FD2Env s v1 v2) [] s),
          MonadPlus (IM (FD2Env s v1 v2) [] s), Ord v) =>
         FDBinding (IM (FD2Env s v1 v2) []) s v


class Container c where
  cmap :: (forall a. t a -> t' a) -> c t -> c t'
  cmapA :: Applicative f =>
           (forall a. (Enum a, Ord a) =>
            t a -> f (t' a)) -> c t -> f (c t')
  cmapM :: Monad m =>
           (forall a. (Enum a, Ord a) =>
            t a -> m (t' a)) -> c t -> m (c t')
  cmapM f = unwrapMonad . cmapA (WrapMonad . f)


newVarState :: FDBinding m s v => [v] -> m s (VarState m s v)
newVarState d = return VarState { varDomain = Set.fromList d
                                , varPropagator = nullPropagator }

newFDVar :: FDBinding m s v => [v] -> m s (FDVar m s v)
newFDVar d = do
  varState <- newVarState d
  newVar varState

newFDVars :: FDBinding m s v => Int -> [v] -> m s [FDVar m s v]
newFDVars n d = replicateM n (newFDVar d)

newFDVarT :: (Traversable t, FDBinding m s v) =>
             t [v] -> m s (t (FDVar m s v))
newFDVarT = Traversable.mapM newFDVar

-- newFDVarC :: (Container c, FDBinding m s v) => c [] -> m s (c (FDVar m s))
-- newFDVarC = cmapM newFDVar

lookupFDVar :: FDBinding m s v => FDVar m s v -> m s (Set v)
lookupFDVar v = do
  modifyFDStat $ \stat -> stat { countLookupVar = countLookupVar stat + 1 }
  vstate <- lookupVar v
  return $ varDomain vstate

updateFDVar :: FDBinding m s v => FDVar m s v -> Set v -> m s ()
updateFDVar v d = do
  modifyFDStat $ \stat -> stat { countUpdateVar' = countUpdateVar' stat + 1 }
  vstate <- lookupVar v
  when (Set.size (varDomain vstate) /= Set.size d) $ do
    modifyFDStat $ \stat -> stat { countUpdateVar = countUpdateVar stat + 1 }
    updateVar v $ vstate { varDomain = d }
    varPropagator vstate

addPropagator :: FDBinding m s v => FDVar m s v -> VarPropagator m s -> m s ()
addPropagator v p = do
  vstate <- lookupVar v
  updateVar v $ vstate { varPropagator = varPropagator vstate >> p }

-- Make arc consistent
addArcPropagator :: (FDBinding m s v1, FDBinding m s v2) =>
                    VarPropagator m s -> FDVar m s v1 -> FDVar m s v2 -> m s ()
addArcPropagator p x y = do
  p
  addPropagator x p
  addPropagator y p

-- Make multi consistent
addMultiPropagator :: (FDBinding m s v) =>
                      VarPropagator m s -> [FDVar m s v] -> m s ()
addMultiPropagator p vs = do
  p
  mapM_ (`addPropagator` p) vs

hasValue :: FDBinding m s v => FDVar m s v -> v -> m s ()
var `hasValue` val = do
  vals <- lookupFDVar var
  guard $ val `Set.member` vals
  updateFDVar var (Set.singleton val)

label :: FDBinding (IM e []) s v => FDVar (IM e []) s v -> IM e [] s v
label var = do
  vals <- lookupFDVar var
  val <- IM . lift $ Set.toList vals
  var `hasValue` val
  return val

labelling :: FDBinding (IM e []) s v => [FDVar (IM e []) s v] -> IM e [] s [v]
labelling = mapM label

labellingT :: (Traversable t, FDBinding (IM e []) s v) =>
              t (FDVar (IM e []) s v) -> IM e [] s (t v)
labellingT = Traversable.mapM label

{-
labellingC :: Container c => c (Var m s) -> IM e m s (c Maybe)
labellingC = cmapM (\var -> label var >>= return . Just)
-}

guardNull :: MonadPlus m => Set v -> m ()
guardNull d = guard $ not $ Set.null d

single :: Set v -> Bool
single s = Set.size s == 1

type ArcPropagator a b = Set a -> Set b -> (Set a, Set b)

arcConstraint :: (FDBinding m s a, FDBinding m s b) =>
                 ArcPropagator a b -> FDVar m s a -> FDVar m s b -> m s ()
arcConstraint c x y = addArcPropagator p x y where
  p = do
    vx <- lookupFDVar x
    vy <- lookupFDVar y
    let (vx', vy') = c vx vy
    guardNull vx'
    updateFDVar x vx'
    guardNull vy'
    updateFDVar y vy'

type MultiPropagator v = [Set v] -> [Set v]

multiConstraint :: (FDBinding m s v) =>
                   MultiPropagator v -> [FDVar m s v] -> m s ()
multiConstraint c xs = addMultiPropagator p xs where
  p = do
    vs <- mapM lookupFDVar xs
    let vs' = c vs
    (`mapM_` zip xs vs') $ \(x, vx') -> do
      guardNull vx'
      updateFDVar x vx'

-- x == y
eq :: FDBinding m s v => FDVar m s v -> FDVar m s v -> m s ()
eq = arcConstraint eqConstraint

eqConstraint :: Ord v => ArcPropagator v v
eqConstraint vx vy = (vz, vz) where
  vz = vx `Set.intersection` vy

-- x <= y
le :: FDBinding m s v => FDVar m s v -> FDVar m s v -> m s ()
le = arcConstraint leConstraint

leConstraint :: Ord v => ArcPropagator v v
leConstraint vx vy = (vx', vy') where
  minX = Set.findMin vx
  maxY = Set.findMax vy
  vx' = Set.filter (<= maxY) vx
  vy' = Set.filter (>= minX) vy

-- x /= y
neq :: FDBinding m s v => FDVar m s v -> FDVar m s v -> m s ()
neq = arcConstraint neqConstraint

neqConstraint :: Ord v => ArcPropagator v v
neqConstraint vx vy
  | single vx && single vy =
    if vx == vy
    then (Set.empty, Set.empty)
    else (vx, vy)
  | single vx && vx `Set.isProperSubsetOf` vy = (vx, vy Set.\\ vx)
  | single vy && vy `Set.isProperSubsetOf` vx = (vx Set.\\ vy, vy)
  | otherwise = (vx, vy)

-- differ from each other
alldiff :: FDBinding m s v => [FDVar m s v] -> m s ()
alldiff []     = return ()
alldiff (v:vs) = do
  mapM_ (v `neq`) vs
  alldiff vs

-- differ from each other
alldiffF :: (Foldable f, FDBinding m s v) => f (FDVar m s v) -> m s ()
alldiffF = alldiff . Foldable.toList

-- x == y (mod m)
eqmod :: (FDBinding m s v, Integral v) =>
         v -> FDVar m s v -> FDVar m s v -> m s ()
eqmod m = arcConstraint (eqmodConstraint m)

eqmodConstraint :: Integral v => v -> ArcPropagator v v
eqmodConstraint m vx vy = (vx', vy') where
  vmz = Set.map (`mod` m) vx `Set.intersection` Set.map (`mod` m) vy
  vx' = Set.filter (\e -> (e `mod` m) `Set.member` vmz) vx
  vy' = Set.filter (\e -> (e `mod` m) `Set.member` vmz) vy

-- x /= y (mod m)
neqmod :: (FDBinding m s v, Integral v) =>
          v -> FDVar m s v -> FDVar m s v -> m s ()
neqmod m = arcConstraint (neqmodConstraint m)

neqmodConstraint :: Integral v => v -> ArcPropagator v v
neqmodConstraint m vx vy = (vx'', vy'') where
  vmx = Set.map (`mod` m) vx
  vmy = Set.map (`mod` m) vy
  vy' = Set.filter (\e -> (e `mod` m) `Set.notMember` vmx) vy
  vx' = Set.filter (\e -> (e `mod` m) `Set.notMember` vmy) vx
  (vx'', vy'')
    | single vmx && single vmy =
      if vmx == vmy
      then (Set.empty, Set.empty)
      else (vx, vy)
    | single vmx && vmx `Set.isProperSubsetOf` vmy = (vx, vy')
    | single vmy && vmy `Set.isProperSubsetOf` vmx = (vx', vy)
    | otherwise = (vx, vy)

-- differ from each other
alldiffmod :: (FDBinding m s v, Integral v) => v -> [FDVar m s v] -> m s ()
alldiffmod _ []     = return ()
alldiffmod m (v:vs) = do
  mapM_ (neqmod m v) vs
  alldiffmod m vs

-- c == x + y (c is constant value)
add :: (FDBinding m s v, Num v) => v -> FDVar m s v -> FDVar m s v -> m s ()
add c = arcConstraint (addConstraint c)

addConstraint :: (Eq v, Num v) => v -> ArcPropagator v v
addConstraint c vx vy = (vx', vy') where
  vx' = Set.filter (\ix -> any (\iy -> ix+iy==c) $ Set.toList vy) vx
  vy' = Set.filter (\iy -> any (\ix -> ix+iy==c) $ Set.toList vx) vy

-- z == x + y
-- x == z - y
-- y == z - x
add3 :: (FDBinding m s v, Num v) =>
        FDVar m s v -> FDVar m s v -> FDVar m s v -> m s ()
add3 z x y = multiConstraint add3Constraint [x, y, z]

add3Constraint :: (Ord v, Num v) => MultiPropagator v
add3Constraint [vx, vy, vz] = [vx', vy', vz'] where
  minZ = Set.findMin vx + Set.findMin vy
  maxZ = Set.findMax vx + Set.findMax vy
  vz' = Set.filter (\e -> minZ <= e && e <= maxZ) vz
  --
  minX = Set.findMin vz - Set.findMax vy
  maxX = Set.findMax vz - Set.findMin vy
  vx' = Set.filter (\e -> minX <= e && e <= maxX) vx
  --
  minY = Set.findMin vz - Set.findMax vx
  maxY = Set.findMax vz - Set.findMin vx
  vy' = Set.filter (\e -> minY <= e && e <= maxY) vy

-- x - y == c (c is constant value)
-- x == y + c
sub :: (FDBinding m s v, Num v) => v -> FDVar m s v -> FDVar m s v -> m s ()
sub c = arcConstraint (subConstraint c)

subConstraint :: (Eq a, Num a) => a -> ArcPropagator a a
subConstraint c vx vy = (vx', vy') where
  vx' = Set.filter (\ix -> any (\iy -> ix==iy+c) $ Set.toList vy) vx
  vy' = Set.filter (\iy -> any (\ix -> ix==iy+c) $ Set.toList vx) vy

--
-- Test Functions
--

test0 :: [[Int]]
test0 = evalFD $ do
  x <- newFDVar [1..3] :: FD s Int (FDVar (IM (FDEnv s Int) []) s Int)
--   x <- newFDVar [1..3]
  y <- newFDVar [4..5]
  labelling [x, y]

{-|
>>> evalFD test1
[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
-}
test1 :: FD s Int [Int]
test1 = do
  x <- newFDVar [1..3]
  y <- newFDVar [4..5]
  labelling [x, y]

{-|
>>> evalFD test2
[[1,3],[2,2],[3,1]]
-}
-- test2 :: Binding m s Int => m s [Int]
test2 :: FD s Int [Int]
test2 = do
  x <- newFDVar [1..3]
  y <- newFDVar [1..3]
  add 4 x y
  labelling [x, y]

{-|
>>> evalFD test3
[[1,3,7],[2,2,8],[3,1,9]]
-}
test3 :: FD s Int [Int]
test3 = do
  x <- newFDVar [1..10]
  y <- newFDVar [1..10]
  z <- newFDVar [1..10]
  add 4 x y
  add 10 y z
  labelling [x, y, z]

{-|
>>> evalFD testSub1
[[1,3],[2,4],[3,5]]
-}
testSub1 :: FD s Int [Int]
testSub1 = do
  x <- newFDVar [1..5]
  y <- newFDVar [1..5]
  sub 2 y x
  labelling [x, y]

{-|
>>> evalFD testEq1
[[1,3,1],[2,4,2],[3,5,3]]
-}
testEq1 :: FD s Int [Int]
testEq1 = do
  x <- newFDVar [1..5]
  y <- newFDVar [1..5]
  z <- newFDVar [1..5]
  z `eq` x
  sub 2 y x
  labelling [x, y, z]

{-|
>>> evalFD testLE1
[[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
-}
testLE1 :: FD s Int [Int]
testLE1 = do
  x <- newFDVar [1..3]
  y <- newFDVar [1..3]
  x `le` y
  labelling [x, y]

{-|
>>> evalFD testNeq1
[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
-}
testNeq1 :: FD s Int [Int]
testNeq1 = do
  x <- newFDVar [1..3]
  y <- newFDVar [1..3]
  x `neq` y
  labelling [x, y]

{-|
>>> length $ evalFD testAlldiff1
6
-}
testAlldiff1 :: FD s Int [Int]
testAlldiff1 = do
  x <- newFDVar [1..3]
  y <- newFDVar [1..3]
  z <- newFDVar [1..3]
  alldiff [x, y, z]
  labelling [x, y, z]

{-|
>>> length $ evalFD testVars1
24
-}
testVars1 :: FD s Int [Int]
testVars1 = do
  xs <- newFDVars 4 [1..4]
  alldiff xs
  labelling xs

{-|
>>> evalFD testAdd31
[[4,1,3],[4,2,2],[5,2,3],[5,3,2],[6,3,3]]
-}
testAdd31 :: FD s Int [Int]
testAdd31 = do
  x <- newFDVar [4..8]
  y <- newFDVar [0..3]
  z <- newFDVar [2..3]
  add3 x y z
  labelling [x, y, z]

{-|
>>> evalFD testAdd32
[[0,0],[0,1],[0,2],[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
-}
testAdd32 :: FD s Int [Int]
testAdd32 = do
  x <- newFDVar [0..3]
  y <- newFDVar [0..3]
  z <- newFDVar [0..2]
  add3 y x z
  labelling [x, y]

{-|
>>> evalFD testEqmod1
[[4,1],[4,4],[5,2],[5,5]]
-}
testEqmod1 :: FD s Int [Int]
testEqmod1 = do
  x <- newFDVar [4..5]
  y <- newFDVar [0..5]
  eqmod 3 x y
  labelling [x, y]

{-|
>>> evalFD testNeqmod1
[[4,0],[4,2],[4,3],[4,5],[5,0],[5,1],[5,3],[5,4]]
-}
testNeqmod1 :: FD s Int [Int]
testNeqmod1 = do
  x <- newFDVar [4..5]
  y <- newFDVar [0..5]
  neqmod 3 x y
  labelling [x, y]

{-|
>>> evalFD testBool1
[[False,True,False],[True,False,True]]
-}
testBool1 :: FD s Bool [Bool]
testBool1 = do
  x <- newFDVar [False, True]
  y <- newFDVar [False, True]
  z <- newFDVar [False, True]
  x `neq` y
  y `neq` z
  labelling [x, y, z]

{-|
Embedding variable into Traversable
>>> evalFD testTraversable
[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
-}
testTraversable :: FD s Int [Int]
testTraversable = do
  vars <- newFDVarT [[1..3], [1..3]]
  alldiffF vars
  labellingT vars

{-|
Test for Constraints with Multiple Type Variables (only simple labelling)
>>> evalFD2 testMultiType0
[(1,False),(1,True),(2,False),(2,True),(3,False),(3,True)]
-}
testMultiType0 :: FD2 s Int Bool (Int, Bool)
testMultiType0 = do
  -- create variables for multiple types
  x <- newFDVar [1..3]
  y <- newFDVar [False, True]
  -- labelling
  vx <- label x
  vy <- label y
  -- construct resolution
  return (vx, vy)

{-|
Example of Constraints with Multiple Type Variables
-}
mt :: (FDBinding m s Int, FDBinding m s Bool) =>
      FDVar m s Int -> FDVar m s Bool -> m s ()
mt = arcConstraint mtConstraint

mtConstraint :: ArcPropagator Int Bool
mtConstraint vx vy = (vx', vy') where
  vx' = if single vy
        then
          if head . Set.toList $ vy
          then Set.filter (\x -> x `mod` 2 == 0) vx
          else Set.filter (\x -> x `mod` 2 /= 0) vx
        else vx
  vy' = if single vx
        then
          let vx1 = head . Set.toList $ vx
          in Set.fromList [vx1 `mod` 2 == 0]
        else vy

{-|
Test for Constraints with Multiple Type Variables
>>> evalFD2 testMultiType1
[(1,False),(2,True),(3,False),(4,True),(5,False)]
-}
testMultiType1 :: FD2 s Int Bool (Int, Bool)
testMultiType1 = do
  -- create variables for multiple types
  x <- newFDVar [1..5]
  y <- newFDVar [False, True]
  -- add constraint for multiple types
  x `mt` y
  -- labelling
  vx <- label x
  vy <- label y
  -- construct resolution
  return (vx, vy)

{-|
Example of Constraints with Multiple Type Variables in Container
-}

newtype PairList x y t = PairList { unPairList :: [(t x, t y)] }
                       deriving (Show, Eq)

instance (Enum x, Ord x, Enum y, Ord y) =>
         Container (PairList x y) where
  cmap f (PairList ps) = PairList $ fmap (\(x, y) -> (f x, f y)) ps
  cmapA f (PairList ps) =
    PairList <$> traverse (\(tx, ty) -> (,) <$> f tx <*> f ty) ps
{-
{-|
Test for Constraints with Multiple Type Variables in Container
>>> length $ evalFD testMultiType2
24
-}
testMultiType2 :: (Binding m s Int, Binding m s Bool) => m s [(Int, Bool)]
testMultiType2 = do
  v <- newFDVarC "PairList" $
       PairList [ ([1..3], [True, False])
                , ([4..5], [True, False]) ]
  --  labelling per type
  ca <- labellingT (map fst (unPairList v))
  cb <- labellingT (map snd (unPairList v))
  return $ zip ca cb

{-|
Test for Constraints with Multiple Type Variables in Container
>>> length $ evalFD testMultiType3
24
-}
testMultiType3 :: (Binding m s Int, Binding m s Bool) => m s (PairList Int Bool Maybe)
testMultiType3 = do
  v <- newFDVarC "PairList" $
       PairList [ ([1..3], [True, False])
                , ([4..5], [True, False]) ]
  labellingC v
-}
