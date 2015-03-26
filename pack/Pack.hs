{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Pack where

import Control.Applicative
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Generics

data Pr (t ∷ k) = Pr
  deriving (Show)

type I (a :: *) = a
type family I' a
type instance I' a = a

type Pair_ (t :: * -> *) a b = (t a, t b)
type PIS t = (t Int, t String)
-- type PairI = Pair I'

class Pack p where
  type F (t :: * -> *) a
  type F Identity a = a
  type F t a = t a
  type L (t :: * -> *) p

type family T (t :: * -> *) a
type instance T t (a, b) = (t a, b)

type App c = forall d b. Data d => c (d -> b) -> d -> c b
type Con c = forall g. g -> c g

app :: Applicative c => App c
app = undefined

con :: Applicative c => Con c
con = pure

gap :: (forall a. a -> t a) -> p -> T t p
gap f p = undefined


class Tr a where
  type Co (c :: * -> *) a
  type Co c a = c a
--   tr :: (forall d. d -> c d) -> a -> Co c a
  tr :: (forall d. d -> c d) -> a -> c a
  tr f = f
-- instance Tr Int where

newtype Pair t a b = Pair (t a, b) deriving (Show, Eq)
deriving instance Typeable Pair
deriving instance (Typeable t, Typeable a, Typeable b, Data (t a), Data b)
                  => Data (Pair t a b)

-- t = everywhere (mkT (+ 1)) (1, (2, 3))
{-
class Typeable a => Data a where
  gfoldl ::
    (forall d b. Data d => c (d -> b) -> d -> c b)
    -> (forall g. g -> c g) -> a -> c a

b -> c b
gmapT -- c = Identity
gmapQ -- c = Constant

f    a     b     c  ...
f (t a) (t b) (t c) ...
f    a'    b'    c' ...

gfoldl app con
con C `app` x_1 `app` x_2 ... `app` x_n
C :: T_1 -> T_2 -> ... -> T_n -> D
con C                                   :: c (T_1 -> T_2 -> ... -> T_n -> D)
con C `app` x_1                         :: c (T_2 -> ... -> T_n -> D)
con C `app` x_1 `app` x_2               :: c (... -> T_n -> D)
con C `app` x_1 `app` x_2 ... `app` x_n :: c D

  gmapT :: (forall b. Data b => b -> b) -> a -> a
  gmapT f x0 = unID (gfoldl k ID x0)
    where
      k :: Data d => ID (d->b) -> d -> ID b
      k (ID c) x = ID (c (f x))

instance Data a => Data [a] where
  gmapT  _   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
-}



class Sat a where
  dict :: a

class Incr a where
  gincr :: a -> a
data IncrD a = Incr { gincrD :: a -> a }
instance Incr a => Sat (IncrD a) where
  dict = Incr { gincrD = gincr }
instance Enum a => Incr a where
  gincr = succ
instance Incr String where
  gincr = (++ "+")

-- | copy from Data.Data
{-|
>>> gmapT_ (mkT $ \x->x+1) (1,2,3)
(2,3,4)
-}
gmapT_ :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT_ f x0 = unID (gfoldl k ID x0)
  where
    k :: Data d => ID (d->b) -> d -> ID b
    k (ID c) x = ID (c (f x))

newtype ID x = ID { unID :: x }

gmapT1 :: (Data (p t), Data (p t'))
       => (forall b. Data b => b -> b)
       -> p t -> p t'
gmapT1 = undefined
-- gmapT1 f x0 = unID (gfoldl k ID x0)
--   where
--     k :: Data d => ID (d->b) -> d -> ID b
--     k (ID c) x = ID (c (f x))

test :: (forall b. Data b => b -> t b) -> a -> t a
test = undefined

j :: Data d => ID (d->b) -> d -> ID b
j (ID c) x = ID (c (id x))


infixr 1 `op`
op :: Int -> Int -> Int
op = (+)

gfoldl_ ::
    Data a
    => (forall d b. Data d => c (d -> b) -> d -> c b)
    -> (forall g. g -> c g)
    -> [a] -> c [a]
gfoldl_ _ z []     = z []
gfoldl_ f z (x:xs) = z (:) `f` x `f` xs


class MMap f where
  mmapM :: Monad m => (forall a. a -> m b) -> f a -> m (f b)
--   mmapM :: Monad m => (a -> m b) -> f a -> m (f b)

instance MMap [] where
  mmapM f as = mapM f as
