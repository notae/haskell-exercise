{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Functor where

import Data.Functor
-- import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Maybe
import Text.Show.Functions

--
-- Deriving Functor
--
{-|
prop> fmap id (P (a, b)) == id (P (a, b))
prop> fmap (g . f) (P (a, b)) == ((fmap g) . (fmap f)) (P (a, b))
-}
newtype P a b = P (a, b) deriving (Show, Eq, Functor)

--
-- Wrapped with newtype
--
{-|
prop> fmap id (PA (P (a, b))) == id (PA (P (a, b)))
prop> fmap (g . f) (PA (P (a, b))) == ((fmap g) . (fmap f)) (PA (P (a, b)))
-}
newtype PA b a = PA (P a b) deriving (Show, Eq)
instance Functor (PA b) where
  fmap f (PA (P (a, b))) = PA (P (f a, b))

--
-- Compose
--
{-|
>>> getCompose $ fmap (+1) $ Compose [Just 1, Nothing, Just 3]
[Just 2,Nothing,Just 4]
>>> getCompose $ getCompose $ fmap (+1) $ Compose $ Compose [[Just 1,Nothing],[Just 3]]
[[Just 2,Nothing],[Just 4]]
-}

--
-- Pair
--
{-|
>>> (\(Pair x y) -> (x, y)) $ fmap (+1) $ Pair [1,2,3] (Just 123)
([2,3,4],Just 124)
-}

--
-- Multiple type parameters in nested types
--
data Point x y = Point { pointX :: x, pointY :: y } deriving (Show, Eq)

newtype PointXY a = PointXY (Point a a) deriving (Show, Eq)
newtype PointX y x = PointX (Point x y) deriving (Show, Eq)
newtype PointY x y = PointY (Point x y) deriving (Show, Eq)

{-|
prop> fmap id (PointXY (Point x y)) == id (PointXY (Point x y))
prop> fmap (g . f) (PointXY (Point x y)) == ((fmap g) . (fmap f)) (PointXY (Point x y))
prop> fmap id (PointX (Point x y)) == id (PointX (Point x y))
prop> fmap (g . f) (PointX (Point x y)) == ((fmap g) . (fmap f)) (PointX (Point x y))
prop> fmap id (PointY (Point x y)) == id (PointY (Point x y))
prop> fmap (g . f) (PointY (Point x y)) == ((fmap g) . (fmap f)) (PointY (Point x y))
-}
instance Functor PointXY where
  fmap f (PointXY (Point x y)) = PointXY $ Point (f x) (f y)
instance Functor (PointX y) where
  fmap f (PointX (Point x y)) = PointX $ Point (f x) y
instance Functor (PointY x) where
  fmap f (PointY (Point x y)) = PointY $ Point x (f y)

{-|
>>> getCompose $ fmap (+1) $ Compose [PointXY $ Point 1 2, PointXY $ Point 3 4]
[PointXY (Point {pointX = 2, pointY = 3}),PointXY (Point {pointX = 4, pointY = 5})]
-}
type PList_ x y = [Point x y]
