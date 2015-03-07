-- Example of Multiple Type Stack on ST Monad

{-# LANGUAGE ExistentialQuantification #-}

module MultiTypeStack where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Control.Monad.ST (runST)
import Data.STRef (STRef)
import Data.STRef (modifySTRef)
import Data.STRef (newSTRef)
import Data.STRef (readSTRef)
import Data.STRef (writeSTRef)


type Var s a = STRef s a

data VarInfo s a = VarInfo { var   :: Var s a
                           , stack :: STRef s [a] }

data AnyVarInfo s = forall a. (Show a) => AnyVarInfo (VarInfo s a)

type VarsInfo s = STRef s [AnyVarInfo s]

newVarsInfo :: ST s (VarsInfo s)
newVarsInfo = newSTRef []

sShow :: AnyVarInfo s -> ST s String
sShow (AnyVarInfo vi) = liftM show $ readSTRef (var vi)

_new :: Show a => VarsInfo s -> a -> ST s (Var s a)
_new vsi a = do
  v <- newSTRef a
  s <- newSTRef []
  modifySTRef vsi $ \vis -> AnyVarInfo (VarInfo v s):vis
  return v

get :: Var s a -> ST s a
get = readSTRef

set :: Var s a -> a -> ST s ()
set = writeSTRef

_push :: VarsInfo s -> ST s ()
_push vsi = do
  vis <- readSTRef vsi
  mapM_ __push vis
  where
    __push :: AnyVarInfo s -> ST s ()
    __push (AnyVarInfo (VarInfo v s)) = do
      a <- readSTRef v
      modifySTRef s $ \as -> a:as

_pop :: VarsInfo s -> ST s ()
_pop vsi = do
  vis <- readSTRef vsi
  mapM_ __pop vis
  where
    __pop :: AnyVarInfo s -> ST s ()
    __pop (AnyVarInfo (VarInfo v s)) = do
      (a:as) <- readSTRef s
      writeSTRef v a
      writeSTRef s as


{-|
>>> testStack
(123,True,LT,["GT","False","456"],["LT","True","123"])
-}
testStack :: (Int, Bool, Ordering, [String], [String])
testStack = runST $ do
  -- setup
  vsi <- newVarsInfo
  let new a = _new vsi a
  let push = _push vsi
  let pop = _pop vsi
  -- create variables with stack and initial values
  vi <- new 123
  vb <- new True
  vo <- new LT
  -- push all variables
  push
  -- modify variables
  set vi 456
  set vb False
  set vo GT
  -- (for debug)
  vis <- readSTRef vsi
  ls <- mapM sShow vis
  -- pop all variables
  pop
  -- get results
  i <- get vi
  b <- get vb
  o <- get vo
  -- (for debug)
  vis' <- readSTRef vsi
  ls' <- mapM sShow vis'
  return (i, b, o, ls, ls')
