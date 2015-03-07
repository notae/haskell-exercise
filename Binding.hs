-- Multiple Type Binding

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Binding where

import           Control.Applicative ((<$>))
import           Control.Monad.State (MonadState, State)
import           Control.Monad.State (get, gets, modify, runState)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Text.Show.Functions ()

type Id = Int
data Var a = Var Id deriving (Show, Eq, Ord)
data NVar = forall a. NVar (Var a)
deriving instance Show NVar
type Val a = (a, [a])
data NVal = forall a. Show a => NVal (Val a)
deriving instance Show NVal

data Binding =
  Binding
  { nextId  :: Id
  , binding :: Map Id NVal }
  deriving (Show)

initBinding :: Binding
initBinding = Binding { nextId = 0, binding = Map.empty }

newVar :: Show a => Val a -> State Binding (Var a)
newVar a = do
  vid <- gets nextId
  modify $ \b -> b { nextId = vid + 1
                   , binding = Map.insert vid (NVal a) (binding b) }
  return $ Var vid

getVar :: Var a -> State Binding NVal
getVar (Var vid) = do
  b <- gets binding
  let (Just va) = Map.lookup vid b
  return va

putVar :: Show a => Var a -> Val a -> State Binding ()
putVar (Var vid) a = do
  modify $ \b -> b { binding = Map.insert vid (NVal a) (binding b) }
  return ()

label :: [NVar] -> State Binding String
label [] = return ""
label ((NVar v):vs) = do
  NVal a <- getVar v
  s <- label vs
  return $ show a ++ ";" ++ s

-- f x      = ..
-- f (x, y) = ..
eval :: (a -> g) -> v -> (v -> Maybe a) -> Maybe g
eval f v e = f <$> (e v)

eval2 :: ((x1, x2) -> g) -> v -> (v -> Maybe a) -> Maybe g
eval2 = undefined

vs :: (Int, Bool)
vs = undefined

f1 (i, b) = show (i + 1) ++ ";" ++ show (not b)
f2 i b = show (i + 1) ++ ";" ++ show (not b)
test2 = (f2, f2 2, f2 2 True)

test = flip runState initBinding $ do
  vx <- newVar (123, [])
  x <- getVar vx
  vy <- newVar (True, [])
  putVar vy (False, [])
  y <- getVar vy
  s <- label [NVar vx, NVar vy]
  return (x, y, s)

select :: NVar -> State Binding ()
select (NVar v) = do
  NVal (a, as) <- getVar v
--   putVar v (head as, as)
  return ()

test4 = flip runState initBinding $ do
  vx <- newVar (0, [1..5])
  select (NVar vx)
  x <- getVar vx
  return x

test3 :: [(Int, Int)]
test3 = ret where
  ret = zip ret2 ret3
  ret2 = proc2 mid
  ret3 = proc3 mid
  mid = proc0
proc0 = [1..10]
proc2 = map (*2)
proc3 = map (*3)

test5 :: (Int, Bool)
test5 = (id 123, id True)

-- type family Apply f
-- type instance Apply (a -> b) =

-- eval :: NVar -> (a -> b) -> State Binding b
-- eval (NVar v) f = do
--   Val a <- getVar v
--   let b = f a
--   return b

{-
class Arg a where
  apply :: (a -> g) -> Vs a -> (Vs a -> a) -> g

instance Arg Int where
  apply f v env = f (env v)

instance Arg (a1, a2) where
  apply f (v1, v2) env = f (env (v1, v2))
-}
{-
class Apply f where
  apply :: f a g -> v -> (v -> a) -> g

instance Apply (->) where
  apply f v env = f (env v)
-}
