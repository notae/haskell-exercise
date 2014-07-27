-- ST Monad Example

import Control.Monad.ST (ST)
import Control.Monad.ST (runST)
import Data.STRef (STRef)
import Data.STRef (modifySTRef)
import Data.STRef (newSTRef)
import Data.STRef (readSTRef)
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import qualified Data.Traversable as Traversable
import Data.Set (Set)
import Data.Set (deleteMin)
import qualified Data.Set as Set

prog :: Int -> ST s Int
prog i = do
  r <- newSTRef i
  modifySTRef r (+ 2)
  modifySTRef r (* 3)
  readSTRef r

test :: Int -> Int
test i = runST (prog i)

-- export error
prog2 :: Int -> ST s (STRef s Int)
prog2 i = do
  newSTRef i
-- test2 :: Int -> STRef s Int
-- test2 i = runST (prog2 i)

-- with Traversable
progT :: (Traversable t, Num a) => t a -> t a
progT t = runST $ do
  -- import into ST
  tr <- Traversable.mapM newSTRef t
  -- do something
  _  <- Traversable.mapM (flip modifySTRef (+ 1)) tr
  -- export from ST
  Traversable.mapM readSTRef tr

-- with List Monad

{-
label :: STRef s [a] -> ST s [a]
label = do
  val <- 

progL :: (Traversable t, Num a) => t (Set a) -> [t a]
progL t = runST $ do
  -- import into ST
  tr  <- Traversable.mapM newSTRef t
  -- do something
  _   <- Traversable.mapM (flip modifySTRef deleteMin) tr
  -- export from ST
  tr' <- Traversable.mapM readSTRef tr
  -- labelling
  Traversable.mapM label tr'
-}
