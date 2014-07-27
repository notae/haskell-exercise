-- Example of storing multiple types into single Map using Enum

import Control.Monad.State (State)
import Control.Monad.State (runState)
import Control.Monad.State (evalState)
import Control.Monad.State (gets)
import Control.Monad.State (modify)
import Control.Monad (liftM2)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map


data EvalState = EvalState { nextVarId :: Int
                           , vars :: Map Int Int
                           } deriving Show

data Var v = Var { varId :: Int } deriving Show

initialState :: EvalState
initialState = EvalState 0 Map.empty

run :: State EvalState v -> (v, EvalState)
run prog = runState prog initialState

eval :: State EvalState v -> v
eval prog = evalState prog initialState

newVar :: (Enum v) => v -> State EvalState (Var v)
newVar val = do
  newId <- gets nextVarId
  modify $ \s -> s { nextVarId = newId + 1 }
  let var = Var newId
  updateVar var val
  return var

lookupVar :: (Enum v) => Var v -> State EvalState v
lookupVar var = do
  vs <- gets vars
  let val = Map.lookup (varId var) vs
  return (toEnum (fromMaybe undefined val))

updateVar :: (Enum v) => Var v -> v -> State EvalState ()
updateVar var val = do
  vs <- gets vars
  modify $ \s -> s { vars = Map.insert (varId var) (fromEnum val) vs }

add :: (Enum v, Num v) => Var v -> Var v -> State EvalState v
add x y = do
  vx <- lookupVar x
  vy <- lookupVar y
  return $ vx + vy

lt :: (Enum v, Ord v) => Var v -> Var v -> State EvalState Bool
lt x y = do
  vx <- lookupVar x
  vy <- lookupVar y
  return $ vx < vy

testProg :: State EvalState (Int, Bool)
testProg = do
  -- Integer
  x <- newVar 2
  y <- newVar 3
  vz <- x `add` y
  z <- newVar vz
  -- Bool
  b <- x `lt` y
  b' <- newVar (not b)
  -- result
  liftM2 (,) (lookupVar z) (lookupVar b')

runTest = run testProg
evalTest = eval testProg
