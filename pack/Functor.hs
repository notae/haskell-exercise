{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Functor where

import Data.Functor
import Data.Functor.Compose
import Data.Functor.Constant
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
-- Data.Functor.Compose
--
{-|
>>> testCompose1
[Just 2,Nothing,Just 4]
>>> testCompose2
[[Just 2,Nothing],[Just 4]]
>>> testCompose3
[[Just 1,Just 2],[Just 3]]
>>> testCompose4
[[1,-1],[3]]
-}
-- nest level 1 to 1
nest1 :: [Maybe Int]
nest1 = [Just 1, Nothing, Just 3]
testCompose1 :: [Maybe Int]
testCompose1 = getCompose $ fmap (+1) $ Compose nest1
-- nest level 2 to 2
nest2 :: [[Maybe Int]]
nest2 = [[Just 1,Nothing],[Just 3]]
testCompose2 :: [[Maybe Int]]
testCompose2 = getCompose $ getCompose $ fmap (+1) $ Compose $ Compose nest2
-- lift up
testCompose3 :: [[Maybe Int]]
testCompose3 = getCompose $ fmap Just $ Compose [[1,2],[3]]
-- lift down
testCompose4 :: [[Int]]
testCompose4 = getCompose $ fmap (fromMaybe (-1)) $ Compose nest2

--
-- Data.Functor.Product
--
{-|
>>> (\(Pair x y) -> (x, y)) $ fmap (+1) $ Pair [1,2,3] (Just 123)
([2,3,4],Just 124)
-}

--
-- Multiple type parameters in nested types
--
data Point x y = Point { pointX :: x, pointY :: y } deriving (Show, Eq)

-- Wrapped types
newtype PointXY a  = PointXY { unPointXY :: (Point a a) } deriving (Show, Eq)
newtype PointX y x = PointX { unPointX :: (Point x y) } deriving (Show, Eq)
newtype PointY x y = PointY { unPointY :: (Point x y) } deriving (Show, Eq)

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

pointXYL :: [PointXY Int]
pointXYL = [PointXY $ Point 1 2, PointXY $ Point 3 4]

{-|
>>> testFmapPointXY
[PointXY {unPointXY = Point {pointX = 2, pointY = 3}},PointXY {unPointXY = Point {pointX = 4, pointY = 5}}]
-}
testFmapPointXY :: [PointXY Int]
testFmapPointXY = (getCompose . fmap (+1) . Compose) pointXYL

pointL :: [Point Int Int]
pointL = [Point 1 2, Point 3 4]

{-|
>>> fmap (fmapX (+1)) pointL
[Point {pointX = 2, pointY = 2},Point {pointX = 4, pointY = 4}]
>>> fmap (fmapY (+1)) pointL
[Point {pointX = 1, pointY = 3},Point {pointX = 3, pointY = 5}]
>>> fmap (fmapXY (+1)) pointL
[Point {pointX = 2, pointY = 3},Point {pointX = 4, pointY = 5}]
>>> fmap (fmapY (*2)) $ fmap (fmapX (+1)) [Point 123 1.2 :: Point Int Double]
[Point {pointX = 124, pointY = 2.4}]
-}
fmapX :: (a -> x) -> Point a y -> Point x y
fmapX f = unPointX . fmap f . PointX
fmapY :: (a -> y) -> Point x a -> Point x y
fmapY f = unPointY . fmap f . PointY
fmapXY :: (a -> b) -> Point a a -> Point b b
fmapXY f = unPointXY . fmap f . PointXY

-- Wrapped Tuple
newtype FT1 b a = FT1 { getFT1 :: (a, b) } deriving (Show, Eq)
instance Functor (FT1 b) where
  fmap f (FT1 (a, b)) = FT1 (f a, b)
newtype FT2 a b = FT2 { getFT2 :: (a, b) } deriving (Show, Eq)
instance Functor (FT2 a) where
  fmap f (FT2 (a, b)) = FT2 (a, f b)

nest5 :: [(String, Maybe Bool)]
nest5 = [("foo", Just True), ("bar", Nothing)]
testCompose5 :: [(String, Maybe Bool)]
testCompose5 = (getCompose . getCompose . fmap not . Compose . Compose) nest5

nest6 :: [(String, Bool)]
nest6 = [("foo", True), ("bar", False)]
-- nmap6 :: [(a, b)] -> [(Maybe a, b)]
nmap6 :: Functor f => (a -> a') -> f (a, b) -> f (a', b)
nmap6 f = fmap getFT1 . getCompose . fmap f . Compose . fmap FT1
testCompose6 :: [(Maybe String, Bool)]
testCompose6 = nmap6 Just nest6
-- nmap7 :: [(a, b)] -> [(a, Maybe b)]
nmap7 :: Functor f => (b -> b') -> f (a, b) -> f (a, b')
nmap7 f = fmap getFT2 . getCompose . fmap f . Compose . fmap FT2
testCompose7 :: [(String, Maybe Bool)]
testCompose7 = nmap7 Just nest6
-- nmap8 :: [(a, b)] -> [(Maybe a, Maybe b)]
nmap8 :: (forall t. t -> Maybe t) -> [(a, b)] -> [(Maybe a, Maybe b)]
nmap8 f = nmap7 f . nmap6 f
testCompose8 :: [(Maybe String, Maybe Bool)]
testCompose8 = nmap8 Just nest6

pair1 :: (String, Bool)
pair1 = ("foo", True)

newtype Wrapped f g a = Wrapped { getWrapped :: f (g a) } deriving Show

-- Recursive fmap
-- rfmap :: Functor f => (forall g. Functor g => g a -> g ) f a -> f b
-- rfmap f = f . fmap f
