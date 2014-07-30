-- Example of Container for Multiple Types

{-# LANGUAGE RankNTypes #-}

module MultiTypeContainer where

import Control.Applicative (Applicative)
import Control.Applicative ((<$>))
import Control.Applicative ((<*>))
import Control.Applicative (WrappedMonad (..))
import Data.Maybe (listToMaybe)
import Data.Traversable (traverse)

class Container c where
  cmap :: (forall a. t a -> t' a) -> c t -> c t'
  cmapA :: Applicative f => (forall a. t a -> f (t' a)) -> c t -> f (c t')
  cmapM :: Monad m => (forall a. t a -> m (t' a)) -> c t -> m (c t')
  cmapM f = unwrapMonad . cmapA (WrapMonad . f)

-- containing 2 types

newtype Pair x y t = Pair { unPair :: (t x, t y) }
                     deriving (Show, Eq)

instance Container (Pair x y) where
  cmap f (Pair (tx, ty)) = Pair (f tx, f ty)
  cmapA f (Pair (tx, ty)) = (\x y -> Pair (x,y)) <$> (f tx) <*> (f ty)

pair :: Pair Int Bool []
pair = Pair ([1..3], [True, False])

testPairCmap :: Pair Int Bool Maybe
testPairCmap  = cmap listToMaybe pair

testPairCmapA :: [Pair Int Bool Maybe]
testPairCmapA = cmapA (map Just) pair

{-|
>>> length testPairCmapM == 3 * 2
True
>>> testPairCmapM
[Pair {unPair = (Just 1,Just True)},Pair {unPair = (Just 1,Just False)},Pair {unPair = (Just 2,Just True)},Pair {unPair = (Just 2,Just False)},Pair {unPair = (Just 3,Just True)},Pair {unPair = (Just 3,Just False)}]
-}
testPairCmapM :: [Pair Int Bool Maybe]
testPairCmapM = cmapM (map Just) pair

-- containing container

data ListC a b c t =
  ListC
  { unListCA :: [t a]
  , unListCB :: [t b]
  , unListCC :: [t c] }
  deriving (Show)

instance Container (ListC a b c) where
  cmap  f (ListC as bs cs) = ListC (fmap f as) (fmap f bs) (fmap f cs)
  cmapA f (ListC as bs cs) =
    ListC <$> traverse f as <*> traverse f bs <*> traverse f cs

listc :: ListC Int Bool Ordering []
listc = ListC [[1..3], [4..5]] [[True, False]] [[LT, GT]]

testListCCmap :: ListC Int Bool Ordering Maybe
testListCCmap  = cmap listToMaybe listc

testListCCmapA :: [ListC Int Bool Ordering Maybe]
testListCCmapA = cmapA (map Just) listc

{-|
>>> length testListCCmapM == 3 * 2 * 2 * 2
True
>>> head testListCCmapM
ListC {unListCA = [Just 1,Just 4], unListCB = [Just True], unListCC = [Just LT]}
>>> last testListCCmapM
ListC {unListCA = [Just 3,Just 5], unListCB = [Just False], unListCC = [Just GT]}
-}
testListCCmapM :: [ListC Int Bool Ordering Maybe]
testListCCmapM = cmapM (map Just) listc

-- containing container

newtype PairList x y t = PairList { unPairList :: [(t x, t y)] }
                       deriving (Show, Eq)

instance (Enum x, Ord x, Enum y, Ord y) =>
         Container (PairList x y) where
  cmap f (PairList ps) = PairList $ fmap (\(x, y) -> (f x, f y)) ps
  cmapA f (PairList ps) =
    PairList <$> traverse (\(tx, ty) -> (,) <$> f tx <*> f ty) ps

pairList :: PairList Integer Bool []
pairList = PairList [ ([1..3], [True, False])
                    , ([4..5], [True, False]) ]

testPairListCmap :: PairList Integer Bool Maybe
testPairListCmap  = cmap listToMaybe pairList

testPairListCmapA :: [PairList Integer Bool Maybe]
testPairListCmapA = cmapA (map Just) pairList

testPairListCmapM :: [PairList Integer Bool Maybe]
testPairListCmapM = cmapM (map Just) pairList