{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Applicative
import Control.Lens
import Control.Lens.Action
import Control.Monad.State
import Data.Maybe

type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup =
  Meetup { _name :: String, _location :: (Latitude, Longitude) }
  deriving (Show, Eq)
makeLenses ''Meetup

{-|
with State monad
>>> testState
(True,Meetup {_name = "foo", _location = (2.0,4.0)})
-}

testState :: (Bool, Meetup)
testState = runState `flip` Meetup "foo" (2, 3) $ do
  location . _2 %= (+1)
  lo <- use (location . _2)
  return $ lo > 1

{-|
>>> [(1,"one"),(2,"two")] ^.. traverse . _2
["one","two"]
-}

testRef :: Lens' Meetup Degrees -> Meetup -> Bool
testRef l s = s ^. l > 2

{-|
>>> runIdentity $ mapMOf traverse (\i -> Identity $ take i $ repeat '*') [1,2,3] :: [String]
["*","**","***"]
-}

-- Traversal
{-|
>>> mapMOf (traverse._2) (\xs -> putStrLn (reverse xs)) [(12,"foo"),(34,"bar")]
oof
rab
[(12,()),(34,())]
>>> mapMOf (both._2) (\xs -> putStrLn (reverse xs)) ((12,"foo"),(34,"bar"))
oof
rab
((12,()),(34,()))
>>> (fromMaybe [] $ [(123,True),(456,False)] & mapMOf (each._1) (\a -> Just (a:[]))) & mapMOf (each._2) (\a -> Just (a:[]))
Just [([123],[True]),([456],[False])]
-}

-- Iso

{-|
>>> Just 123 ^. someIso
Right 123
>>> Right 123 ^. from someIso
Just 123
-}
someIso :: Iso' (Maybe a) (Either () a)
someIso = iso f g where
  f (Just a) = Right a
  f Nothing = Left ()
  g (Left _) = Nothing
  g (Right a) = Just a

type NEList a = (a, [a])

{-|
>>> (1,[2,3]) ^. neIso
1
>>> 1 ^. from neIso
(1,[])
-}
neIso :: Iso' (a, [a]) a
neIso = iso f g where
  f (a, _) = a
  g a = (a, [])

{-|
>>> [1,2,3] ^. oneIso
Just 1
>>> Just 1 ^. from oneIso
[1]
>>> [] ^. oneIso
Nothing
>>> Nothing ^. from oneIso
[]
-}
oneIso :: Iso' [a] (Maybe a)
oneIso = iso f g where
  f = listToMaybe
  g = maybeToList

type PairList a b = [(a, b)]
pl1 :: PairList Int Bool
pl1 = [(1, True), (2, False)]

{-|
>>> pl2
Just [(1,True),(2,False)]
>>> pl3
Just [(2,True),(3,False)]
>>> pl4
Just [(1,False),(2,True)]
>>> pl5
Just [(2,False),(3,True)]

>>> pl1 & traverseOf (each . _1) %~ Just
[(Just 1,True),(Just 2,False)]
-}

pl2 :: Maybe (PairList Int Bool)
pl2 = pl1 & traverseOf (each . _1) Just
inc1 :: PairList Int Bool -> Maybe (PairList Int Bool)
inc1 = traverseOf (each . _1) (Just . succ)
pl3 :: Maybe [(Int, Bool)]
pl3 = pl1 & inc1
not2 :: PairList Int Bool -> Maybe (PairList Int Bool)
not2 = traverseOf (each . _2) (Just . not)
pl4 :: Maybe [(Int, Bool)]
pl4 = pl1 & not2
pl5 :: Maybe (PairList Int Bool)
pl5 = (pl1 & inc1) >>= not2

{-|
{-|
>>> plMap show pl1
[("1","True"),("2","False")]
-}
plMap :: (forall e. e -> f e) -> PairList a b -> PairList (f a) (f b)
plMap f = tr _2 . tr _1  where
  tr l = traverseOf (each . l) %~ f

{-|
>>> plLift pl1 :: PairList [Int] [Bool]
[([1],[True]),([2],[False])]
-}
plLift :: Applicative f => PairList a b -> PairList (f a) (f b)
plLift = plMap pure
-}


pld1 :: PairList [Int] [Bool]
pld1 = [([1, 2, 3], [True, False])
       ,([2, 3, 4], [True, False])]

plTraverse :: Applicative f
           => (forall x. x -> f x) -> PairList a b -> f (PairList a b)
plTraverse f = traverse (tupleTraverse f)
-- plTraverse f t = tr2 . tr1 t where
--   tr1 = traverseOf (each . _1) f
--   tr2 = traverseOf (each . _2) f

tupleTraverse :: Applicative f
              => (forall x. x -> f x) -> (a, b) -> f (a, b)
tupleTraverse f (a, b) = (,) <$> f a <*> f b

testTT :: Maybe (Int, Bool)
testTT = tupleTraverse (Just) (1, False)


-- liftdown

type PLiftIso a b f = Iso' (a, b) (f a, f b)

type LDIso t f a = Iso' (t (f a)) (f (t a))

type LiftDown a f m = forall f. f (m a) -> m (f a)
