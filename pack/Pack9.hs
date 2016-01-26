{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pack9 where

import Control.Applicative
import Data.Functor.Identity
import Data.Maybe
import Text.Show.Functions

import Data.List (sort)
import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Exts as GHCExts

import Control.Lens
import Control.Natural

-- Data types

--   System-defined 2-Tuple

type Tuple_ a b f = (f a, f b)
newtype TupleV a b f = TupleV { getTupleV :: Tuple_ a b f } deriving (Show, Eq)

--   User-defined

data Pair x y = Pair { _px :: x,  _py :: y } deriving (Show, Eq)

makeLenses ''Pair

type Pair_ x y f = Pair (f x) (f y)

newtype PairV x y f = PairV { getPairV :: Pair_ x y f }
                     deriving (Show, Eq)

type PairI = Pair Int Int

instance (t ~ PairV x y f) => Rewrapped (PairV x y g) t
instance Wrapped (PairV x y f) where
  type Unwrapped (PairV x y f) = Pair_ x y f
  _Wrapped' = iso getPairV PairV
  {-# INLINE _Wrapped' #-}

-- Example values

p1 :: Pair Int Bool
p1 = Pair 2 True

p2 :: Pair_ Int Bool [] -- Pair [Int] [Bool]
p2 = Pair [1..3] [True, False]

t2 :: ([Int], [Bool])
t2 = ([1..3], [True, False])

{-|
>>> p3 ^. _Wrapped' == p2
True
>>> p2 ^. _Unwrapped' == p3
True
>>> p3 ^. _Wrapped'
Pair {_px = [1,2,3], _py = [True,False]}
>>> p3 ^. _Wrapped' . px
[1,2,3]
-}
p3 :: PairV Int Bool []
p3 = PairV p2

-- Functions accepting lenses

tr :: Lens s t x x' -> s -> (x -> x') -> t
tr l p f = p & l %~ f

-- Using type synonyms and no foralls

type NT' s t f g a = forall (m :: * -> *). Applicative m =>
                     (f a -> m (g a)) -> s -> m t

pntx' :: NT' (Pair (f x) (f y)) (Pair (g x) (f y)) f g x
pntx' f (Pair x y) = Pair <$> f x <*> pure y

pnty' :: NT' (Pair (g x) (f y)) (Pair (g x) (g y)) f g y
pnty' f (Pair x y) = Pair <$> pure x <*> f y

{-|
>>> runIdentity $ pnty' (Identity . listToMaybe) $ runIdentity $ pntx' (Identity . listToMaybe) p2
Pair {_px = Just 1, _py = Just True}
-}

pnt'' :: (f x -> g x) -> (f y -> g y) -> Pair_ x y f -> Pair_ x y g
pnt'' f g = runIdentity . pnty' (Identity . g) . runIdentity . pntx' (Identity . f)

pntM'' :: Applicative m
          => (f x -> m (g x)) -> (f y -> m (g y))
          -> Pair_ x y f -> m (Pair_ x y g)
pntM'' fx fy (Pair x y) = Pair <$> fx x <*> fy y

type GNT s t (f :: * -> *) (g :: * -> *) (m :: * -> *) =
  Applicative m => s -> m t

pgntM :: (f x -> m (g x)) -> (f y -> m (g y)) ->
         GNT (Pair_ x y f) (Pair_ x y g) f g m
pgntM = pntM''

pgLToS :: (Ord x, Ord y) => GNT (Pair_ x y []) (Pair_ x y Set) [] Set Identity
pgLToS = pgntM (pure . Set.fromList) (pure. Set.fromList)

polyfn :: Ord a => [a] -> Set a
polyfn = Set.fromList

pgLToS' :: (Ord x, Ord y)
           => (forall a. Ord a => [a] -> Set a)
           -> GNT (Pair_ x y []) (Pair_ x y Set) [] Set Identity
pgLToS' f = pgntM (pure . f) (pure. f)

-- Using type synonyms and foralls

pnt' :: (forall a. f a -> g a) -> Pair_ x y f -> Pair_ x y g
pnt' f = runIdentity . pnty' (Identity . f) . runIdentity . pntx' (Identity . f)

pntM' :: Applicative m
          => (forall a. f a -> m (g a))
          -> Pair_ x y f -> m (Pair_ x y g)
pntM' f (Pair x y) = Pair <$> f x <*> f y

--   NT-like

type NT s t f g = forall (m :: * -> *). Applicative m =>
                  (forall a. f a -> m (g a)) -> s -> m t

pnt :: NT (Pair (f x) (f y)) (Pair (g x) (g y)) f g
pnt f (Pair x y) = Pair <$> f x <*> f y

--   unlifting

type Unlift l b m = Applicative m => l -> m b

punlift :: Unlift (Pair (m x) (m y)) (Pair x y) m
punlift (Pair x y) = Pair <$> x <*> y

--   usecase

listToMaybe' :: NT s t [] Maybe -> s -> t
listToMaybe' nt s = runIdentity $ nt (Identity . listToMaybe) s

{-|
>>> lToMUnlift pnt punlift p2
Just (Pair {_px = 1, _py = True})
-}
lToMUnlift :: NT s l [] Maybe -> Unlift l b Maybe -> s -> Maybe b
lToMUnlift nt unlift = unlift . listToMaybe' nt

-- Natural Transformation

length' :: [a] -> Const Int a
length' as = Const (length as)

-- Context Extention to Control.Natural

infixr 0 ~~>
type (f ~~> g) c = forall a. c a => f a -> g a

infixr 0 :~~>, $$$
newtype (f :~~> g) c = CNat { ($$$) :: (f ~~> g) c }

cnt1 :: ([] ~~> Set) Ord
cnt1 = Set.fromList

cntTest :: Set Int
cntTest = (CNat (Set.fromList :: ([] ~~> Set) Ord) :: ([] :~~> Set) Ord) $$$ [1,2,3]

cntTest2 :: Set Int
cntTest2 = (CNat Set.fromList :: ([] :~~> Set) Ord) $$$ [1,2,3]

cnt3 :: ([] :~~> Set) Ord
cnt3 = CNat Set.fromList

cntTest3 :: Set Int
cntTest3 = cnt3 $$$ [1,2,3]

-- NG: for ambiguity check
-- pcnt0 :: (c x, c y) => (f ~~> g) c -> Pair (f x) (f y) -> Pair (g x) (g y)
-- pcnt0 f (Pair x y) = Pair (f x) (f y)

-- pcntTest = pcnt cnt1 p2

-- OK: with variable type context in newtype
pcnt1 :: (c x, c y) => (f :~~> g) c -> Pair (f x) (f y) -> Pair (g x) (g y)
pcnt1 f (Pair x y) = Pair (f $$$ x) (f $$$ y)

{-|
>>> pcntTest1
Pair {_px = fromList [1,2,3], _py = fromList [False,True]}
-}
pcntTest1 :: Pair_ Int Bool Set
pcntTest1 = pcnt1 cnt3 p2

-- NG: with variable type context outside newtype
-- pcnt' :: (c x, c y) => (forall a. c a => f a -> g a)
--          -> Pair (f x) (f y) -> Pair (g x) (g y)
-- pcnt' f (Pair x y) = Pair (f x) (f y)

-- OK: without type context
pcnt'' :: (forall a. f a -> g a) -> Pair (f x) (f y) -> Pair (g x) (g y)
pcnt'' f (Pair x y) = Pair (f x) (f y)

-- OK: with type context fixed to Ord
pcnt''' :: (Ord x, Ord y) => (forall a. Ord a => f a -> g a)
         -> Pair (f x) (f y) -> Pair (g x) (g y)
pcnt''' f (Pair x y) = Pair (f x) (f y)

-- NG: for ambiguity check
-- fnforall :: forall c g. (c Int) => (forall a. c a => [a] -> g a) -> [Int] -> g Int
-- fnforall f as = f as

-- NT-like transformation

infixr 0 ~->
type (f ~-> g) c m = forall a. c a => f a -> m (g a)

infixr 0 :~->, $-
newtype (f :~-> g) c m = CNT { ($-) :: (f ~-> g) c m }

pcnt :: (c x, c y, Applicative m) =>
        (f :~-> g) c m -> Pair (f x) (f y) -> m (Pair (g x) (g y))
pcnt f (Pair x y) = Pair <$> (f $- x) <*> (f $- y)

cnt4 :: ([] :~-> Set) Ord Identity
cnt4 = CNT (Identity . Set.fromList)

pcntTest2 :: Pair_ Int Bool Set
pcntTest2 = runIdentity $ pcnt cnt4 p2

class IsCNT f g where
  type NTTyCxt (f :: * -> *) (g :: * -> *) a :: GHCExts.Constraint
  type NTCxt (f :: * -> *) (g :: * -> *) :: * -> *


-- NT-like transformation over structure

class Applicative m => GCNT s t f g c m | s -> f, t -> g where
  gcntA :: (f :~-> g) c m -> s -> m t

instance (c a, c b, Applicative m) =>
         GCNT (f a, f b) (g a, g b) f g c m where
  gcntA cntA (a, b) = (,) <$> (cntA $- a) <*> (cntA $- b)

{-|
>>> testGNT1
(fromList [1,2,3],fromList [False,True])
-}
testGNT1 :: (Set Int, Set Bool)
testGNT1 = runIdentity $ gcntA cnt4 t2

testGNT2 :: (Set Int, Set Bool)
testGNT2 = runIdentity $ gcntA (CNT (Identity . Set.fromList) :: ([] :~-> Set) Ord Identity) t2

-- NT-like transformation over parameterized structure

class Applicative m => PGCNT t f g c m where
  pgcntA :: (f :~-> g) c m -> t f -> m (t g)

instance (c a, c b, Applicative m) =>
         PGCNT (PairV a b) f g c m where
  pgcntA cntA (PairV (Pair a b)) =
    PairV <$> (Pair <$> (cntA $- a) <*> (cntA $- b))

{-|
>>> testPGNT1
PairV {getPairV = Pair {_px = fromList [1,2,3], _py = fromList [False,True]}}
-}
testPGNT1 :: PairV Int Bool Set
testPGNT1 = runIdentity $ pgcntA cnt4 p3

testPGNT2 :: Pair_ Int Bool Set
testPGNT2 = runIdentity (pgcntA cnt4 (p2 ^. _Unwrapping' PairV)) ^. _Wrapped'

{-|
>>> testPGNT3
Pair {_px = fromList [1,2,3], _py = fromList [False,True]}
-}
testPGNT3 :: Pair_ Int Bool Set
testPGNT3 = under (_Wrapping PairV) (runIdentity . pgcntA cnt4) p2

-- NG: The type variable ‘t0’ is ambiguous
-- testPGNT4 :: Pair_ Int Bool Set
-- testPGNT4 = under _Wrapped (runIdentity . pgcntA cnt4) p2

{-|
>>> under (_Wrapping PairV) testPGNT5 p2
Pair {_px = fromList [1,2,3], _py = fromList [False,True]}
-}
testPGNT5 :: PGCNT t [] Set Ord Identity => t [] -> t Set
testPGNT5 = runIdentity . pgcntA cnt4
