module RWST where

import Control.Monad.RWS
import Control.Monad.ST.Lazy
import Data.STRef.Lazy

--
-- RWS monad
--

test1 :: (Int, Int, [String])
test1 = runRWS testRWS [("one", 1), ("two", 2)] 10

testRWS :: RWS [(String, Int)] [String] Int Int
testRWS = do
  tell ["start"]
  i <- asks (lookup "one")
  tell ["val:" ++ show i]
  tell ["end"]
  return $ maybe 999 id i

--
-- RWST monad with ST monad
--

type Env = [(String, Int)]
type Log = [String]

initEnv :: [(String, Int)]
initEnv = [("one", 1), ("two", 2)]

test2 :: (Int, Int, [String])
test2 = runST $ runRWST testRWSST initEnv 10

testRWSST :: RWST Env Log Int (ST s) Int
testRWSST = do
  tell ["start"]
  i <- asks (lookup "one")
  tell ["val:" ++ show i]
  v <- lift $ newSTRef i
  v' <- lift $ readSTRef v
  tell ["var:" ++ show v']
  tell ["end"]
  return $ maybe 999 id i


data Env2 s =
  Env2
  { binding :: [(String, Int)]
  , var     :: Maybe (STRef s Int)
  }

initEnv2 :: Env2 s
initEnv2 = Env2 [("one", 1), ("two", 2)] Nothing

test3 :: (Int, Int, [String])
test3 = runST $ runRWST testRWSST2 initEnv2 10

testRWSST2 :: RWST (Env2 s) Log Int (ST s) Int
testRWSST2 = do
  tell ["start"]
  i <- asks (lookup "one" . binding)
  let i' = maybe 999 id i
  tell ["val:" ++ show i']
  v <- lift $ newSTRef i'
  i2 <- local (\e -> e { var = Just v }) $ do
    Just v2 <- asks var
    lift $ readSTRef v2
  tell ["var:" ++ show i2]
  tell ["end"]
  return i2
