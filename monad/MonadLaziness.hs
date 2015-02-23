-- Example of monad laziness

module MonadLaziness where

import Control.Monad (forM)
import Control.Monad.ST (runST)
import Control.Monad.State (get, put, runState)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Debug.Trace (traceM)

{-|
>>> head $ testST 3
Start:1
End:1
Start:2
End:2
Start:3
End:3
0
-}
testST :: Int -> [Int]
testST n = runST $ do
  v <- newSTRef 0
  a <- forM [1..n] $ \i -> do
    traceM $ "Start:" ++ show i
    s <- readSTRef v
    writeSTRef v (s + i)
    traceM $ "End:" ++ show i
    return s
  return a

{-|
>>> head $ fst $ testState 1000000000
Start:1
0
-}
testState :: Int -> ([Int], Int)
testState n = flip runState 0 $ do
  a <- forM [1..n] $ \i -> do
    traceM $ "Start:" ++ show i
    s <- get
    put (s + i)
    traceM $ "End:" ++ show i
    return s
  return a

{-|
>>> head $ testST2 1
*1
*2
1
-}
testST2 :: Int -> [Int]
testST2 _ = runST $ do
  v <- newSTRef 0
  traceM "*1"
  writeSTRef v 1
  a1 <- readSTRef v
  traceM "*2"
  writeSTRef v 2
  a2 <- readSTRef v
  return [a1, a2]

{-|
>>> head $ fst $ testState2 1
1
-}
testState2 :: Int -> ([Int], Int)
testState2 _ = flip runState 0 $ do
  traceM "*1"
  put 1
  a1 <- get
  traceM "*2"
  put 2
  a2 <- get
  return [a1, a2]

{-|
>>> head $ testMaybe2 1
*1
*2
1
-}
testMaybe2 :: Int -> [Int]
testMaybe2 _ = maybe [] id $ do
  traceM "*1"
  a1 <- Just 1
  traceM "*2"
  a2 <- Just 2
  return [a1, a2]
