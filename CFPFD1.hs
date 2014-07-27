{-|
Module      : CFPFD1
Description : Constraint Functional Programming over Multiple Finite Domain
Copyright   : (c) notae@me.com, 2014
License     : BSD3
Maintainer  : notae@me.com
Stability   : experimental
Portability : POSIX

This module provides interfaces for constraint programming
over multiple finite domain in Haskell.
Originally from: <http://overtond.blogspot.jp/2008/07/pre.html>
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CFPFD1
       (
       -- * Monads
         FD
       , FDStat (..)
       , evalFD
       , runFD
       -- * Variables
       , Var
       , newVar
       , newVars
       , newVarT
       -- * Constraint Store
       , hasValue
       -- (for defining custom constraints)
       , ArcPropagator
       , arcConstraint
       , MultiPropagator
       , multiConstraint
       -- * Labelling
       , label
       , labelling
       , labellingT
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
       ) where

import Control.Monad (MonadPlus)
import Control.Monad (guard)
import Control.Monad (when)
import Control.Monad (replicateM)
import Control.Monad.State (MonadState)
import Control.Monad.State (StateT)
import Control.Monad.State (evalStateT)
import Control.Monad.State (get)
import Control.Monad.State (gets)
import Control.Monad.State (put)
import Control.Monad.State (runStateT)
import Control.Monad.State (modify)
import Control.Monad.State (lift)
import Control.Applicative (Applicative)
import Control.Applicative (Alternative)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable


newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
                 deriving (Functor, Applicative, Alternative,
                           Monad, MonadPlus, MonadState (FDState s))

data FDState s = FDState { fdNextVarId :: VarId
                         , fdVarSpace  :: VarSpace s
                         , fdStat      :: FDStat }
               deriving (Show)

data Var s a = Var { varId   :: VarId
                   , varName :: VarName }
             deriving (Show, Eq, Ord)

type VarId = Int
type VarName = String

type VarSpace s = IntMap (VarState s)
data VarState s = VarState { varDomain     :: VarDomain,
                             varPropagator :: VarPropagator s }
                  deriving (Show)

type VarDomain = Set Int
type VarPropagator s = FD s ()
instance Show (VarPropagator s) where
  show _ = "*"

data FDStat = FDStat
              { countLookupVar  :: Int
              , countUpdateVar  :: Int
              , countUpdateVar' :: Int }
            deriving (Show)

initialStat :: FDStat
initialStat = FDStat { countLookupVar  = 0
                     , countUpdateVar  = 0
                     , countUpdateVar' = 0 }

evalFD :: (forall s . FD s a) -> [a]
evalFD fd = evalStateT (unFD fd) initialState

runFD :: (forall s . FD s a) -> [(a, FDStat)]
runFD fd = map (\(v, s) -> (v, fdStat s)) ss where
  ss = runStateT (unFD fd) initialState

initialState :: FDState s
initialState = FDState { fdNextVarId = 0, fdVarSpace = IntMap.empty
                       , fdStat = initialStat }

newVar :: Enum a => String -> [a] -> FD s (Var s a)
newVar name dom = do
  var <- nextVar name
  setDomain var dom
  return var
  where
    nextVar :: String -> FD s (Var s a)
    nextVar n = do
      s <- get
      let i = fdNextVarId s
      put $ s { fdNextVarId = i + 1 }
      return Var { varId = i, varName = n }
    setDomain :: Enum a => Var s a -> [a] -> FD s ()
    setDomain v d =
      modify $ \s ->
        let sp = fdVarSpace s
            vstate = VarState { varDomain = Set.fromList . map fromEnum $ d
                              , varPropagator = return () }
        in s { fdVarSpace = IntMap.insert (varId v) vstate sp }

newVars :: Enum a => String -> Int -> [a] -> FD s [Var s a]
newVars name n d = replicateM n (newVar (name ++ ":" ++ show n) d)

newVarT :: (Traversable t, Enum a) => String -> t [a] -> FD s (t (Var s a))
newVarT name = Traversable.mapM (newVar name)

getVarState :: Var s a -> FD s (VarState s)
getVarState v = do
  vspace <- gets fdVarSpace
  return $ vspace IntMap.! (varId v)

putVarState :: Var s a -> VarState s -> FD s ()
putVarState v vstate = do
  vspace <- gets fdVarSpace
  modify $ \s -> s { fdVarSpace = IntMap.insert (varId v) vstate vspace }

modifyFDStat :: (FDStat -> FDStat) -> FD s ()
modifyFDStat f = modify $ \s -> s { fdStat = f (fdStat s) }

lookupVar :: (Ord a, Enum a) => Var s a -> FD s (Set a)
lookupVar v = do
  modifyFDStat $ \stat -> stat { countLookupVar = countLookupVar stat + 1 }
  vstate <- getVarState v
  return $ Set.fromList . map toEnum . Set.toList $ varDomain vstate

updateVar :: (Ord a, Enum a) => Var s a -> Set a -> FD s ()
updateVar v d = do
  modifyFDStat $ \stat -> stat { countUpdateVar' = countUpdateVar' stat + 1 }
  vstate <- getVarState v
  when (Set.size (varDomain vstate) /= Set.size d) $ do
    modifyFDStat $ \stat -> stat { countUpdateVar = countUpdateVar stat + 1 }
    putVarState v $ vstate { varDomain = Set.fromList . map fromEnum . Set.toList $ d }
    varPropagator vstate

addPropagator :: Var s a -> VarPropagator s -> FD s ()
addPropagator v p = do
  vstate <- getVarState v
  putVarState v (vstate { varPropagator = varPropagator vstate >> p })

-- Make arc consistent
addArcPropagator :: VarPropagator s -> Var s a -> Var s b -> FD s ()
addArcPropagator p x y = do
  p
  addPropagator x p
  addPropagator y p

-- Make multi consistent
addMultiPropagator :: VarPropagator s -> [Var s a] -> FD s ()
addMultiPropagator p vs = do
  p
  mapM_ (`addPropagator` p) vs

hasValue :: (Ord a, Enum a) => Var s a -> a -> FD s ()
var `hasValue` val = do
  vals <- lookupVar var
  guard $ val `Set.member` vals
  updateVar var (Set.singleton val)

label :: (Ord a, Enum a) => Var s a -> FD s a
label var = do
  vals <- lookupVar var
  val <- FD . lift $ Set.toList vals
  var `hasValue` val
  return val

labelling :: (Ord a, Enum a) => [Var s a] -> FD s [a]
labelling = mapM label

labellingT :: (Traversable t, Ord a, Enum a) => t (Var s a) -> FD s (t a)
labellingT = Traversable.mapM label

guardNull :: MonadPlus m => Set a -> m ()
guardNull d = guard $ not $ Set.null d

single :: Set a -> Bool
single s = Set.size s == 1

type ArcPropagator a b = Set a -> Set b -> (Set a, Set b)

arcConstraint :: (Ord a, Enum a, Ord b, Enum b) =>
                 ArcPropagator a b -> Var s a -> Var s b -> FD s ()
arcConstraint c x y = addArcPropagator p x y where
  p = do
    vx <- lookupVar x
    vy <- lookupVar y
    let (vx', vy') = c vx vy
    guardNull vx'
    updateVar x vx'
    guardNull vy'
    updateVar y vy'

type MultiPropagator a = [Set a] -> [Set a]

multiConstraint :: (Ord a, Enum a) => MultiPropagator a -> [Var s a] -> FD s ()
multiConstraint c xs = addMultiPropagator p xs where
  p = do
    vs <- mapM lookupVar xs
    let vs' = c vs
    (`mapM_` zip xs vs') $ \(x, vx') -> do
      guardNull vx'
      updateVar x vx'

-- x == y
eq :: (Ord a, Enum a) => Var s a -> Var s a -> FD s ()
eq = arcConstraint eqConstraint

eqConstraint :: (Ord a) => ArcPropagator a a
eqConstraint vx vy = (vz, vz) where
  vz = vx `Set.intersection` vy

-- x <= y
le :: (Ord a, Enum a) => Var s a -> Var s a -> FD s ()
le = arcConstraint leConstraint

leConstraint :: (Ord a) => ArcPropagator a a
leConstraint vx vy = (vx', vy') where
  minX = Set.findMin vx
  maxY = Set.findMax vy
  vx' = Set.filter (<= maxY) vx
  vy' = Set.filter (>= minX) vy

-- x /= y
neq :: (Ord a, Enum a) => Var s a -> Var s a -> FD s ()
neq = arcConstraint neqConstraint

neqConstraint :: (Ord a) => ArcPropagator a a
neqConstraint vx vy
  | single vx && single vy =
    if vx == vy
    then (Set.empty, Set.empty)
    else (vx, vy)
  | single vx && vx `Set.isProperSubsetOf` vy = (vx, vy Set.\\ vx)
  | single vy && vy `Set.isProperSubsetOf` vx = (vx Set.\\ vy, vy)
  | otherwise = (vx, vy)

-- differ from each other
alldiff :: (Ord a, Enum a) => [Var s a] -> FD s ()
alldiff []     = return ()
alldiff (v:vs) = do
  mapM_ (v `neq`) vs
  alldiff vs

-- differ from each other
alldiffF :: (Foldable f, Ord a, Enum a) => f (Var s a) -> FD s ()
alldiffF = alldiff . Foldable.toList

-- x == y (mod m)
eqmod :: Integral a => a -> Var s a -> Var s a -> FD s ()
eqmod m = arcConstraint (eqmodConstraint m)

eqmodConstraint :: Integral a => a -> ArcPropagator a a
eqmodConstraint m vx vy = (vx', vy') where
  vmz = Set.map (`mod` m) vx `Set.intersection` Set.map (`mod` m) vy
  vx' = Set.filter (\e -> (e `mod` m) `Set.member` vmz) vx
  vy' = Set.filter (\e -> (e `mod` m) `Set.member` vmz) vy

-- x /= y (mod m)
neqmod :: Integral a => a -> Var s a -> Var s a -> FD s ()
neqmod m = arcConstraint (neqmodConstraint m)

neqmodConstraint :: Integral a => a -> ArcPropagator a a
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
alldiffmod :: Integral a => a -> [Var s a] -> FD s ()
alldiffmod _ []     = return ()
alldiffmod m (v:vs) = do
  mapM_ (neqmod m v) vs
  alldiffmod m vs

-- c == x + y (c is constant value)
add :: (Ord a, Enum a, Num a) => a -> Var s a -> Var s a -> FD s ()
add c = arcConstraint (addConstraint c)

addConstraint :: (Eq a, Num a) => a -> ArcPropagator a a
addConstraint c vx vy = (vx', vy') where
  vx' = Set.filter (\ix -> any (\iy -> ix+iy==c) $ Set.toList vy) vx
  vy' = Set.filter (\iy -> any (\ix -> ix+iy==c) $ Set.toList vx) vy

-- z == x + y
-- x == z - y
-- y == z - x
add3 :: (Ord a, Enum a, Num a) => Var s a -> Var s a -> Var s a -> FD s ()
add3 z x y = multiConstraint add3Constraint [x, y, z]

add3Constraint :: (Ord a, Num a) => MultiPropagator a
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
sub :: (Ord a, Enum a, Num a) => a -> Var s a -> Var s a -> FD s ()
sub c = arcConstraint (subConstraint c)

subConstraint :: (Eq a, Num a) => a -> ArcPropagator a a
subConstraint c vx vy = (vx', vy') where
  vx' = Set.filter (\ix -> any (\iy -> ix==iy+c) $ Set.toList vy) vx
  vy' = Set.filter (\iy -> any (\ix -> ix==iy+c) $ Set.toList vx) vy

--
-- Test Functions
--

{-|
>>> evalFD test1
[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
-}
test1 :: FD s [Int]
test1 = do
  x <- newVar "X" [1..3]
  y <- newVar "Y" [4..5]
  labelling [x, y]

{-|
>>> evalFD test2
[[1,3],[2,2],[3,1]]
-}
test2 :: FD s [Int]
test2 = do
  x <- newVar "X" [1..3]
  y <- newVar "Y" [1..3]
  add 4 x y
  labelling [x, y]

{-|
>>> evalFD test3
[[1,3,7],[2,2,8],[3,1,9]]
-}
test3 :: FD s [Int]
test3 = do
  x <- newVar "X" [1..10]
  y <- newVar "Y" [1..10]
  z <- newVar "Z" [1..10]
  add 4 x y
  add 10 y z
  labelling [x, y, z]

{-|
>>> evalFD testSub1
[[1,3],[2,4],[3,5]]
-}
testSub1 :: FD s [Int]
testSub1 = do
  x <- newVar "X" [1..5]
  y <- newVar "Y" [1..5]
  sub 2 y x
  labelling [x, y]

{-|
>>> evalFD testEq1
[[1,3,1],[2,4,2],[3,5,3]]
-}
testEq1 :: FD s [Int]
testEq1 = do
  x <- newVar "X" [1..5]
  y <- newVar "Y" [1..5]
  z <- newVar "Z" [1..5]
  z `eq` x
  sub 2 y x
  labelling [x, y, z]

{-|
>>> evalFD testLE1
[[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
-}
testLE1 :: FD s [Int]
testLE1 = do
  x <- newVar "X" [1..3]
  y <- newVar "Y" [1..3]
  x `le` y
  labelling [x, y]

{-|
>>> evalFD testNeq1
[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
-}
testNeq1 :: FD s [Int]
testNeq1 = do
  x <- newVar "X" [1..3]
  y <- newVar "Y" [1..3]
  x `neq` y
  labelling [x, y]

{-|
>>> length $ evalFD testAlldiff1
6
-}
testAlldiff1 :: FD s [Int]
testAlldiff1 = do
  x <- newVar "X" [1..3]
  y <- newVar "Y" [1..3]
  z <- newVar "Z" [1..3]
  alldiff [x, y, z]
  labelling [x, y, z]

{-|
>>> length $ evalFD testVars1
24
-}
testVars1 :: FD s [Int]
testVars1 = do
  xs <- newVars "Xn" 4 [1..4]
  alldiff xs
  labelling xs

{-|
>>> evalFD testAdd31
[[4,1,3],[4,2,2],[5,2,3],[5,3,2],[6,3,3]]
-}
testAdd31 :: FD s [Int]
testAdd31 = do
  x <- newVar "X" [4..8]
  y <- newVar "Y" [0..3]
  z <- newVar "Z" [2..3]
  add3 x y z
  labelling [x, y, z]

{-|
>>> evalFD testAdd32
[[0,0],[0,1],[0,2],[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]
-}
testAdd32 :: FD s [Int]
testAdd32 = do
  x <- newVar "X" [0..3]
  y <- newVar "Y" [0..3]
  z <- newVar "Z" [0..2]
  add3 y x z
  labelling [x, y]

{-|
>>> evalFD testEqmod1
[[4,1],[4,4],[5,2],[5,5]]
-}
testEqmod1 :: FD s [Int]
testEqmod1 = do
  x <- newVar "X" [4..5]
  y <- newVar "Y" [0..5]
  eqmod 3 x y
  labelling [x, y]

{-|
>>> evalFD testNeqmod1
[[4,0],[4,2],[4,3],[4,5],[5,0],[5,1],[5,3],[5,4]]
-}
testNeqmod1 :: FD s [Int]
testNeqmod1 = do
  x <- newVar "X" [4..5]
  y <- newVar "Y" [0..5]
  neqmod 3 x y
  labelling [x, y]

{-|
>>> evalFD testBool1
[[False,True,False],[True,False,True]]
-}
testBool1 :: FD s [Bool]
testBool1 = do
  x <- newVar "X" [False, True]
  y <- newVar "Y" [False, True]
  z <- newVar "Z" [False, True]
  x `neq` y
  y `neq` z
  labelling [x, y, z]

{-|
Embedding variable into Traversable
>>> evalFD testTraversable
[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
-}
testTraversable :: FD s [Int]
testTraversable = do
  vars <- newVarT "T" [[1..3], [1..3]]
  alldiffF vars
  labellingT vars

{-|
Test for Constraints with Multiple Type Variables (only simple labelling)
>>> evalFD testMultiType0
[(1,False),(1,True),(2,False),(2,True),(3,False),(3,True)]
-}
testMultiType0 :: FD s (Int, Bool)
testMultiType0 = do
  -- create variables for multiple types
  x <- newVar "X" [1..3]
  y <- newVar "Y" [False, True]
  -- labelling
  vx <- label x
  vy <- label y
  -- construct resolution
  return (vx, vy)

{-|
Example of Constraints with Multiple Type Variables
-}
mt :: Var s Int -> Var s Bool -> FD s ()
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
>>> evalFD testMultiType1
[(1,False),(2,True),(3,False),(4,True),(5,False)]
-}
testMultiType1 :: FD s (Int, Bool)
testMultiType1 = do
  -- create variables for multiple types
  x <- newVar "X" [1..5]
  y <- newVar "Y" [False, True]
  -- add constraint for multiple types
  x `mt` y
  -- labelling
  vx <- label x
  vy <- label y
  -- construct resolution
  return (vx, vy)
