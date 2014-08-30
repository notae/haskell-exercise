-- StateT with ST

module StateT_ST where

import Control.Monad.ST (ST)
import Control.Monad.ST (runST)
import Control.Monad.State (StateT)
import Control.Monad.State (evalStateT)
import Control.Monad.State (get)
import Control.Monad.State (put)
import Control.Monad.Trans (lift)
import Data.STRef (STRef)
import Data.STRef (modifySTRef)
import Data.STRef (newSTRef)
import Data.STRef (readSTRef)


type StateST s s' = StateT (STRef s s') (ST s)

test :: Int
test = runST $ flip evalStateT undefined $ do
  ginit 123
  gpp
  gget

ginit :: Int -> StateST s Int ()
ginit i = do
  vi <- lift $ newSTRef i
  put vi

gpp :: StateST s Int ()
gpp = do
  vi <- get
  lift $ modifySTRef vi (+1)

gget :: StateST s Int Int
gget = do
  vi <- get
  lift $ readSTRef vi
