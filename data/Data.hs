{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data where

import Data.Generics
import Data.Maybe

data X =
  X
  { foo :: Int
  , bar :: Char
  } deriving (Typeable, Data, Show, Eq)

x0 :: X
x0 = X 123 'a'

{-|
>>> gmapT incr x0
X {foo = 124, bar = 'a'}
-}
incr :: forall d. Data d => d -> d
incr d = case cast d of
          Nothing          -> d
          Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> gmapT incr y0
Y {baz = True, qux = 457, yx = X {foo = 123, bar = 'a'}}
-}

{-|
>>> gmapT (gmapT incr) y0
Y {baz = True, qux = 456, yx = X {foo = 124, bar = 'a'}}
-}

{-|
>>> gmapT (gmapT incr) (gmapT incr y0)
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}

data Y =
  Y
  { baz :: Bool
  , qux :: Int
  , yx  :: X
  } deriving (Typeable, Data, Show, Eq)

y0 :: Y
y0 = Y True 456 x0

{-|
>>> gmapT incr' y0
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
-}
incr' :: forall d. Data d => d -> d
incr' d = case cast d of
           Nothing          -> gmapT incr' d
           Just (i :: Int)  -> fromJust (cast (i + 1))

{-|
>>> gmapT' incr y0
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
>>> gmapT' incr [1::Int,2,3]
[2,3,4]
-}
gmapT' :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT' f = f . gmapT (gmapT' f)

{-|
>>> gmapT'' incr y0
Y {baz = True, qux = 457, yx = X {foo = 124, bar = 'a'}}
>>> gmapT'' incr [1::Int,2,3]
[2,3,4]
-}
gmapT'' :: Data a => (forall b. Data b => b -> b) -> a -> a
gmapT'' f = gmapT (gmapT' f) . f

-- modify per user-defined data
{-|
>>> gmapT' (mkT modX) y0
Y {baz = True, qux = 456, yx = X {foo = 124, bar = 'a'}}
-}
modX :: X -> X
modX x@X{..} = x { foo = foo + 1 }

{-|
>>> gmapT' (visitX modX) y0
Y {baz = True, qux = 456, yx = X {foo = 124, bar = 'a'}}
-}
visitX :: forall d. Data d => (X -> X) -> d -> d
visitX f d = case cast d of
              Nothing -> d
              Just (x :: X) -> fromJust (cast (f x))
