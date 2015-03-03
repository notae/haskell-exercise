{-# LANGUAGE DeriveDataTypeable  #-}
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

f :: forall d. Data d => d -> d
f d = case cast d of
       Nothing          -> d
       Just (i :: Int)  -> fromJust (cast (i + 1))

test :: X
test = gmapT f x

data Y =
  Y
  { baz :: Bool
  , qux :: Int
  , yx  :: X
  } deriving (Typeable, Data, Show, Eq)

y :: Y
y = Y True 456 x

test2 :: Y
test2 = gmapT f y
test3 :: Y
test3 = gmapT (gmapT f) y
test4 :: Y
test4 = gmapT (gmapT f) (gmapT f y)

f' :: forall d. Data d => d -> d
f' d = case cast d of
       Nothing          -> gmapT f' d
       Just (i :: Int)  -> fromJust (cast (i + 1))

test5 :: Y
test5 = gmapT f' y
