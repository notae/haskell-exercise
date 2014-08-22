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


-- stack

type Manager s = STRef s (ST s ())

data Stack s a = Stack { unStack :: [a]
                       , manager :: Manager s }
emptyStack :: Manager s -> Stack s a
emptyStack m = Stack [] m

newManager :: ST s (Manager s)
newManager = newSTRef (return ())

new :: Manager s -> ST s (STRef s (Stack s a))
new m = newSTRef (emptyStack m)

push :: STRef s (Stack s a) -> a -> ST s ()
push r a = do
  s <- readSTRef r
  writeSTRef r s { unStack = a:(unStack s) }
  let rm = manager s
  modifySTRef rm $ \m -> m >> pop r >> return ()

pop :: STRef s (Stack s a) -> ST s a
pop r = do
  Stack (a:as) _ <- readSTRef r
  modifySTRef r $ \s -> s { unStack = as }
  return a

rewind :: Manager s -> ST s ()
rewind rm = do
  m <- readSTRef rm
  m

testStack :: ([Int], [Bool])
testStack = runST $ do
  m <- newManager
  i <- new m
  b <- new m
  push i 123
  push b True
  push i 456
  push b False
  rewind m
  Stack asi _ <- readSTRef i
  Stack asb _ <- readSTRef b
  return (asi, asb)
