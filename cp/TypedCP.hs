{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module TypedCP where

import Data.Typeable
import GHC.Exts (Constraint)
import GHC.TypeLits
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

{-

cf. sized array: Array (n::Nat)

-}

ix, iy :: Int
ix = 2
iy = 3


{-
data Expr (l::L) a c where
  V :: a -> Expr A a c
  B :: Bool -> Expr A a c
  (:+) :: Expr A a c -> Expr A a c -> Expr A a c
  V2 :: a -> c -> Expr C a c

data L = A | C

-- instance Show a => Show (Expr a) where
--   show (V a) = "V=" ++ show a
--   show (B b) = "B=" ++ show b
--   show (x :+ y) = enc (show x) ++ "+" ++ enc (show y)
--   show (V2 a c) = "V2=" ++ show (a, c)

instance Show a => Show (Expr A a c) where
  show (V a) = "V=" ++ show a
  show (B b) = "B=" ++ show b
  show (x :+ y) = enc (show x) ++ "+" ++ enc (show y)

instance (Show a, Show c) => Show (Expr C a c) where
  show (V2 a c) = "V2=" ++ show (a, c)
-}

enc :: String -> String
enc s = "(" ++ s ++ ")"

data W (a :: k) = W deriving Show

f1 :: (p a) => Proxy p -> (a -> String) -> a -> String
f1 _ f = f

f2 :: (a -> String) -> a -> String
f2 f = f

class (Show a, Ord a) => FDValue a
instance FDValue Int

f3 :: FDValue a => a -> String
f3 a = show a ++ ":" ++ show (a == a)

data S' = forall a. Show a => S' a
deriving instance Show S'

data S a = Show a => S a | Eq a => SE
deriving instance Show (S a)

showS :: S a -> String
showS (S a) = "S:" ++ show a
showS SE = "SE"

-- require extention DatatypeContexts (is going to be removed from the language)
-- data Show a => S2 = S2 a

class NT' s t f g where
  nt' :: (forall a. Ord a => f a -> g a) -> s -> t

instance (Ord a, Ord b) => NT' (f a, f b) (g a, g b) f g where
  nt' f (a, b) = (f a, f b)

testNT' :: (Set Int, Set Bool)
testNT' = nt' fn ([1::Int], [True])

fn5 :: Set Int
fn5 = Set.fromList [1]

-- f4 :: Num a => a -> String
-- f4 a = show a

-- class NT' s t (f :: * -> *) (g :: * -> *) where
--   nt' :: (forall a. c a => f a -> g a) -> s -> t

-- instance NT' (f a, f b) (g a, g b) f g where
--   nt' f (a, b) = (f a, f b)

class NT s t f g (c :: * -> Constraint) | s -> f, t -> g, g -> c where
  nt :: (forall a. c a => f a -> g a) -> s -> t

-- instance (c a, c b) => NT (f a, f b) (g a, g b) f g c where
--   nt f (a, b) = (f a, f b)

type family Cxt (f :: * -> *) :: * -> Constraint
type instance Cxt Set = (Ord)

-- instance ((Cxt g) a, (Cxt g) b) => NT (f a, f b) (g a, g b) f g (Cxt g) where
--   nt f (a, b) = (f a, f b)

-- instance (c a, c b) => NT (f a, f b) (g a, g b) f g c where
--   nt f (a, b) = (f a, f b)

instance (Ord a, Ord b) => NT (f a, f b) (g a, g b) f g Ord where
  nt f (a, b) = (f a, f b)

fn :: Ord a => [a] -> Set a
fn = Set.fromList

testNT :: (Set Int, Set Bool)
testNT = nt fn ([1::Int], [True])

fn2 :: (Ord a, Show a) => [a] -> Set a
fn2 = Set.fromList

-- testNT2 :: (Set Int, Set Bool)
-- testNT2 = nt fn2 ([1::Int], [True])


-- type class for abstract collections

class Collection c cxt | c -> cxt where
  member :: cxt a => a -> c a -> Bool

instance Collection [] Eq where
  member = elem

instance Collection Set Ord where
  member = Set.member

-- type class dictionary

data Dict c where
  Dict :: c => Dict c

foo :: Dict c -> (c => r) -> r
foo Dict x = x

bar :: Dict ()
bar = Dict

main1 :: IO ()
main1 = print $ foo bar "Hello"

dictOrd :: Ord a => Dict (Ord a)
dictOrd = Dict
