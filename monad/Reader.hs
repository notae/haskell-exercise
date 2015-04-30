module Reader where

import Control.Monad.Reader

data Env =
  Env
  { flag    :: Bool
  , binding :: [(String, Int)]
  } deriving (Show)

initEnv :: Env
initEnv = Env True [("one", 1), ("two", 2)]

test = runReader `flip` initEnv $ do
  asks (lookup "two" . binding)
