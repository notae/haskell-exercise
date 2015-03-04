{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data where

import Data.Data
import Data.Maybe

data X =
  X
  { foo :: Int
  , bar :: Char
  } deriving (Typeable, Data, Show, Eq)

x :: X
x = X 123 'a'

{-|
>>> gmapT incr x
X {foo = 124, bar = 'a'}
-}
incr :: forall d. Data d => d -> d
incr d = case cast d of
          Nothing          -> d
          Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> gmapT incr y
Y {baz = True, qux = 457, yx = X {foo = 123, bar = 'a'}}
-}

{-|
>>> gmapT (gmapT incr) y
Y {baz = True, qux = 456, yx = X {foo = 124, bar = 'a'}}
-}

{-|
>>> gmapT (gmapT incr) (gmapT incr y)
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}

data Y =
  Y
  { baz :: Bool
  , qux :: Int
  , yx  :: X
  } deriving (Typeable, Data, Show, Eq)

y :: Y
y = Y True 456 x

{-|
>>> gmapT incr' y
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}
incr' :: forall d. Data d => d -> d
incr' d = case cast d of
           Nothing          -> gmapT incr' d
           Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> gmapT' incr y
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
>>> gmapT' incr [1::Int,2,3]
[2,3,4]
-}
gmapT' :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT' f = f . gmapT (gmapT' f)

{-|
>>> gmapT'' incr y
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
>>> gmapT'' incr [1::Int,2,3]
[2,3,4]
-}
gmapT'' :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT'' f = gmapT (gmapT' f) . f
