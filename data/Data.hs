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

incr :: forall d. Data d => d -> d
incr d = case cast d of
          Nothing          -> d
          Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> test
X {foo = 124, bar = 'a'}
-}
test :: X
test = gmapT incr x

data Y =
  Y
  { baz :: Bool
  , qux :: Int
  , yx  :: X
  } deriving (Typeable, Data, Show, Eq)

y :: Y
y = Y True 456 x

{-|
>>> test2
Y {baz = True, qux = 457, yx = X {foo = 123, bar = 'a'}}
-}
test2 :: Y
test2 = gmapT incr y

{-|
>>> test3
Y {baz = True, qux = 456, yx = X {foo = 124, bar = 'a'}}
-}
test3 :: Y
test3 = gmapT (gmapT incr) y

{-|
>>> test4
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}
test4 :: Y
test4 = gmapT (gmapT incr) (gmapT incr y)

incr' :: forall d. Data d => d -> d
incr' d = case cast d of
           Nothing          -> gmapT incr' d
           Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> test5
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}
test5 :: Y
test5 = gmapT incr' y

gmapT' :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT' f = gmapT (f . gmapT f)

{-|
>>> test6
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}
test6 :: Y
test6 = gmapT' incr y
