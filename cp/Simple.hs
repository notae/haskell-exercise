{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Simple where

import           Control.Applicative (Alternative, Applicative)
import           Control.Monad       (MonadPlus)
import           Control.Monad.State (MonadState, StateT)
import qualified Control.Monad.State as S
import           Text.Show.Functions ()

newtype FD s v a =
  FD { unFD :: StateT (FDState s v) [] a }
  deriving (Functor, Applicative, Alternative,
            Monad, MonadPlus, MonadState (FDState s v))

runFD :: v -> FD s v a -> [(a, FDState s v)]
runFD v = flip S.runStateT (initState v) . unFD

data FDState s v =
  FDState
  { fdSpace :: v
  , fdStat  :: FDStat }
  deriving (Show)

type VarId = Int
type FDStat = ()

type Dom = []

-- for each of the element types
class FDSpace v a where
  type Ref v a
  initial :: v Dom
  refs :: v Dom -> v (Ref v)
  readRef :: v Dom -> Ref v a -> a
  writeRef :: v Dom -> Ref v a -> a -> v Dom
  -- TBD: label
  label :: v Dom -> v Dom
  -- TBD: propagator
--   consRef :: v Dom -> Ref v a ->

-- TBD: constraints
-- TBD: propagation queue/stack

initState :: v -> FDState s v
initState v =
  FDState
  { fdSpace = v
  , fdStat = ()
  }

data Var m s v a =
  Var
  { get   :: FD s v [a]
  , set   :: [a] -> FD s v ()
  , props :: [FD s v ()]
  }

-- label :: Var m s v a -> a
-- label = undefined

--
-- Examples
--

data UserData_ b i =
  UserData
  { userBool :: b
  , userInt  :: i
  } deriving (Show)
type UserData = UserData_ Bool Int
type UserDataD = UserData_ [Bool] [Int]

data UserDataRef = BoolRef | IntRef deriving (Show, Eq)

instance FDSpace UserData_ Bool where
--   type Ref v Bool =

initUserState :: UserDataD
initUserState = UserData [True, False] [1..5]

vb :: Var m s UserDataD Bool
vb =
  Var
  { get = S.gets $ userBool . fdSpace
  , set = \b -> S.modify $ \s -> s { fdSpace = (fdSpace s) { userBool = b } }
  , props = []
  }

-- testFD :: FD s UserDataD UserDataD
testFD = do
  S.gets fdSpace

-- test :: [(UserDataD, FDState s UserDataD)]
test = runFD initUserState testFD
