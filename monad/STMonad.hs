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
import Control.Monad (replicateM_)
import Data.STRef (writeSTRef)

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

-- with Set and List Monad

propagate :: (Traversable t, Num a) => t (Set a) -> t (Set a)
propagate t = runST $ do
  -- import into ST
  tr  <- Traversable.mapM newSTRef t
  -- do something
  _   <- Traversable.mapM (flip modifySTRef deleteMin) tr
  -- export from ST
  tr' <- Traversable.mapM readSTRef tr
  return tr'

-- label :: STRef s [a] -> ST s [a]
-- label = undefined

label :: Set a -> [a]
label s = Set.toList s

labelling :: (Traversable t, Num a) => t (Set a) -> [t a]
labelling ts = Traversable.mapM label ts


dup :: STRef s a -> ST s (STRef s a)
dup r = do
  v <- readSTRef r
  newSTRef v

progL :: ST s [[Int]]
progL = do
  rs <- Traversable.mapM newSTRef [[1..3], [1..5]]
  -- duplicate
  rs' <- Traversable.mapM (\r -> readSTRef r >>= newSTRef) rs
  -- labelling
  Traversable.mapM readSTRef rs'
  -- export
  -- vs' <- Traversable.mapM readSTRef rs'
  -- return vs'


-- Multiple type stacks

data Manager s = Manager { pushs :: ST s ()
                         , pops  :: ST s () }

newManager :: ST s (STRef s (Manager s))
newManager = newSTRef $ Manager (return ()) (return ())

-- | Create a new stack
new :: STRef s (Manager s) -> a -> ST s (STRef s [a])
new rm a = do
  r <- newSTRef [a]
  modifySTRef rm $ \m -> m { pushs = pushs m >> pushV r
                           , pops  = pops  m >> pop_V r }
  return r

-- | Set value at top of stack
set :: STRef s [a] -> a -> ST s ()
set r a = do
  modifySTRef r $ \(_:as) -> a:as

-- | Get value at top of stack
get :: STRef s [a] -> ST s a
get r = do
  (a:_) <- readSTRef r
  return a

-- | Duplicate value at top of stack
pushV :: STRef s [a] -> ST s ()
pushV r = do
  modifySTRef r $ \as@(a:_) -> a:as

-- | Remove value at top of stack
pop_V :: STRef s [a] -> ST s ()
pop_V r = do
  modifySTRef r $ \(_:as) -> as

-- | Push stacks in the manager
push :: STRef s (Manager s) -> ST s ()
push rm = do
  m <- readSTRef rm
  pushs m

-- | Pop stacks in the manager
pop :: STRef s (Manager s) -> ST s ()
pop rm = do
  m <- readSTRef rm
  pops m

testStack :: (([Int], [Bool]), (Int, Bool), ([Int], [Bool]), (Int, Bool))
testStack = runST $ do
  -- setup manager
  m <- newManager
  let newm = new m

  -- create stacks
  i <- newm 123
  b <- newm True

  -- push stacks
  push m
  set i 456
  set b False
  si <- readSTRef i
  sb <- readSTRef b
  vi <- get i
  vb <- get b

  -- pop stacks
  pop m
  si' <- readSTRef i
  sb' <- readSTRef b
  vi' <- get i
  vb' <- get b

  -- unsafe pop
--   pop m -- pop to empty
--   si'' <- readSTRef i
--   pop m -- pop from empty
--   si''' <- get i

  return ((si, sb), (vi, vb), (si', sb'), (vi', vb'))
