-- ListT Example

module ListT where

import Control.Monad.List   (ListT (..))
import Control.Monad.State  (State)
import Control.Monad.State  (modify)
import Control.Monad.Trans  (lift)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer (tell)

liftList :: (Monad m) => [a] -> ListT m a
liftList = ListT . return

-- ListT + State
{-|
S=2^1 + ... + 2^n
>>> runState (runListT (replicateM 0 listState)) 0
([[]],0)
>>> runState (runListT (replicateM 1 listState)) 0
([[0],[1]],2)
>>> runState (runListT (replicateM 2 listState)) 0
([[0,0],[0,1],[1,0],[1,1]],6)
>>> runState (runListT (replicateM 3 listState)) 0
([[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]],14)
-}
listState :: ListT (State Int) Int
listState = do
  x <- liftList [0, 1]
  modify (+ 1)
  return x

-- ListT + Writer
--   lift manually since ListT is not an instance of MonadWriter
{-|
>>> runWriter (runListT (replicateM 3 listWriter))
([[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]],["<","x=0>","x=1>","<","x=0>","x=1>","<","x=0>","x=1>","<","x=0>","x=1>","<","x=0>","x=1>","<","x=0>","x=1>","<","x=0>","x=1>"])
-}
listWriter :: ListT (Writer [String]) Int
listWriter = do
  lift $ tell ["<"]
  x <- liftList [0, 1]
  lift $ tell ["x=" ++ show x ++ ">"]
  return x
