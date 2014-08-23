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


data Stack s = forall a. (Show a) => Stack (STRef s [a])

sShow :: Stack s -> ST s String
sShow (Stack r) = liftM show $ readSTRef r

new :: Show a => STRef s [Stack s] -> a -> ST s (STRef s [a])
new rvs a = do
  r <- newSTRef [a]
  modifySTRef rvs $ \vs -> Stack r:vs
  return r

get :: STRef s [a] -> ST s a
get r = do
  (a:_) <- readSTRef r
  return a

set :: STRef s [a] -> a -> ST s ()
set r a = modifySTRef r $ \(_:as) -> a:as

push :: Stack s -> ST s ()
push (Stack r) = modifySTRef r $ \as@(a:_) -> a:as

pushs :: STRef s [Stack s] -> ST s ()
pushs rvs = do
  vs <- readSTRef rvs
  mapM_ push vs

testStack :: ([Int], [Bool], [Ordering], [String])
testStack = runST $ do
  rvs <- newSTRef []
  let newr a = new rvs a
  -- create stacks with initial values
  ri <- newr 123
  rb <- newr True
  ro <- newr LT
  -- push all stacks
  pushs rvs
  -- modify value at the top of stacks
  set ri 456
  set rb False
  set ro GT
  -- get results
  li <- readSTRef ri
  lb <- readSTRef rb
  lo <- readSTRef ro
  ss <- readSTRef rvs
  ls <- mapM sShow ss
  return (li, lb, lo, ls)
