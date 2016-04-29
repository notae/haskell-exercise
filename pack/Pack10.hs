{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pack10 where

import Control.Lens
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Proxy

--
-- Type Classes
--

type family R (r :: * -> *) a

-- Tuple
type instance R f (Int, Bool) = (R f Int, R f Bool)
class PairSym r where
  pair :: Int -> Bool -> r (R r (Int, Bool))

--
-- Instances
--

-- Identity

type instance R Identity Int = Int
type instance R Identity Bool = Bool

instance PairSym Identity where
  pair a b = Identity (a, b)

-- List

newtype L a = L { unL :: a } deriving Show

-- type instance R L a = [a]
type instance R L Int = [Int]
type instance R L Bool = [Bool]

instance PairSym L where
  pair a b = L (pure a, pure b)

-- Maybe

newtype M a = M { unM :: a } deriving Show

-- type instance R M (a, b) = (Maybe a, Maybe b)

type instance R M Int = Maybe Int
type instance R M Bool = Maybe Bool

instance PairSym M where
  pair a b = M (pure a, pure b)

{-|
>>> pair 1 True :: Identity (Int, Bool)
Identity (1,True)
>>> pair 1 True :: L ([Int], [Bool])
L {unL = ([1],[True])}
>>> pair 1 True :: M (Maybe Int, Maybe Bool)
M {unM = (Just 1,Just True)}
-}

-- Translate a representaion to another

i1 :: Identity (Int, Bool)
i1 = Identity (1, True)

-- With lenses



-- With type classes

class Trans r1 r2 a where
  trans :: Proxy a -> r1 (R r1 a) -> r2 (R r2 a)

instance Trans Identity L (Int, Bool) where
  trans _ (Identity (a, b)) = L (pure a, pure b)

-- til1 :: L (R L (Int, Bool))
til1 :: L ([Int], [Bool])
til1 = trans (Proxy :: Proxy (Int, Bool)) i1

-- class Tr s t a b where
--   tr :: (a -> f b) -> s -> f t

-- instance Tr (Identity (Int, Bool)) (L ([Int], [Bool])) where
--   tr f (Identity (a, b)) = Identity $ L $ (,) <$> f (pure a) <*> f (pure b)

--
--
--

-- The base type
data D = A | B Int | C Int Bool deriving Show

-- Add context to the whole type
type D' f = f D

-- Add context to all leaf types
data D1 f = A1 | B1 (f Int) | C1 (f Int) (f Bool)
deriving instance (Show (f Int), Show (f Bool)) => Show (D1 f)
ds1 :: [D1 []]
ds1 = [A1, B1 [1, 2], C1 [1] [True], C1 [2] [False]]
ds1' :: [D]
ds1' = [A, B 1, B 2, C 1 True, C 2 False]

-- Add context to only leaf Int
data D2 f = A2 | B2 (f Int) | C2 (f Int) Bool

-- Add context to only leaf Bool
data D3 f = A3 | B3 Int | C3 Int (f Bool)

-- Parameterize all types
data DP a b = AP | BP a | CP a b deriving Show
-- Isomorphic type as D
type DP0 = DP Int Bool
-- Add context to all leaf types
type DP1 f = DP (f Int) (f Bool)
-- dps1 :: [DP [Int] [Bool]]
dps1 :: [DP1 []]
dps1 = [AP, BP [1, 2], CP [1] [True], CP [2] [False]]
dps1' :: [DP Int Bool]
dps1' = [AP, BP 1, BP 2, CP 1 True, CP 2 False]
-- Add context to some type
type DP2 f = DP (f Int) Bool
type DP3 f = DP Int (f Bool)
