{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Pack10 where

import Control.Natural
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe
import GHC.Exts              (Constraint)
import Text.Show.Functions   ()

-- | NT-like transformation with type context
infixr 0 ~~>
type (f ~~> g) c = forall a. c a => f a -> g a

infixr 0 :~~>, $$$
newtype (f :~~> g) c = CNat { ($$$) :: (f ~~> g) c }

-- | NT-like monadic transformation with type context
infixr 0 ~->
type (f ~-> g) c m = forall a. c a => f a -> m (g a)

infixr 0 :~->, $-
newtype (f :~-> g) c m = CNT { ($-) :: (f ~-> g) c m }

-- | NT-like monadic transformation with type context
class CNT f g where
  type NTTyCxt (f :: * -> *) (g :: * -> *) a :: Constraint
  type NTCxt (f :: * -> *) (g :: * -> *) :: * -> *

{-
-- | NT-like transformation over structure
class GCNT s where
  type Dest (f :: * -> *) s
  gcnt :: (f ~-> g) c m -> s -> Dest f s

instance GCNT (a, b) where
  type Dest f (a, b) = (Dest f a, Dest f b)
--   gcnt (a, b) =
-}

tupleNT :: (f ~> g) -> (f a, f b) -> (g a, g b)
tupleNT f (a, b) = (f a, f b)

tupleLift :: Applicative f => (a, b) -> (f a, f b)
tupleLift (a, b) = (pure a, pure b)

tupleUnlift :: Applicative f => (f a, f b) -> f (a, b)
tupleUnlift (a, b) = (,) <$> a <*> b

testTupleNT :: (Maybe Int, Maybe Bool)
testTupleNT = tupleNT listToMaybe ([1, 2], [True])

testTupleLift :: ([Int], [Bool])
testTupleLift = tupleLift (1, True)

testTupleUnlift :: [(Int, Bool)]
testTupleUnlift = tupleUnlift ([1], [True])


class P p where
  type L p (f :: * -> *)
  plift :: Applicative f => (forall a. a -> f a) -> p -> L p f
  pdown :: Applicative f => L p f -> f p

{-
instance P a where
  type L a f = f a
  plift f a = f a
  pdown a = a
-}

instance P Bool where
  type L Bool f = f Bool
  plift f a = f a
  pdown a = a

instance P Int where
  type L Int f = f Int
  plift f a = f a
  pdown a = a

instance (P a, P b) => P (a, b) where
  type L (a, b) f = (L a f, L b f)
  plift f (a, b) = (plift f a, plift f b)
  pdown (a, b) = (,) <$> pdown a <*> pdown b

testPLift :: ([Int], [Bool])
testPLift = plift pure (1::Int, True)

testPDown :: [(Int, Bool)]
testPDown = pdown ([1], [True])
