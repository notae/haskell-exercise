{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pack6 where

import Data.Functor.Identity
import Data.Maybe

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Lens


s1 :: ([Int], [Bool])
s1 = ([1], [True])

pl1 :: [(Int, Bool)]
pl1 = [(1, True), (2, False)]

test11 :: (Set Int, [Bool])
test11 = over _1 Set.fromList s1

test12 :: ([Int], Set Bool)
test12 = over _2 Set.fromList s1

-- NG
mapTuple1 :: (forall x. f x -> g x) -> (f a, f b) -> (g a, f b)
mapTuple1 f = over _1 f

-- OK
mapTuple2 :: (f a -> g a) -> (f b -> g b) -> (f a, f b) -> (g a, g b)
mapTuple2 f g = over _2 g . over _1 f

-- OK
mapTuple :: (Ord a, Ord b) =>
            (forall x. Ord x => f x -> g x) -> (f a, f b) -> (g a, g b)
mapTuple f = over _2 f . over _1 f

test1 :: (Set Int, Set Bool)
test1 = mapTuple Set.fromList s1

