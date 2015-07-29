{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pack8 where

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified          GHC.Exts as Exts
import Control.Applicative
import Data.Functor.Identity
import Data.Maybe

-- translation targets

l1 :: [Int]
l1 = [1, 2, 3]

p1 :: ([Int], [Bool])
p1 = ([1, 2], [True, False])

-- NT-like transformation

class Applicative (NTCxt f g) => NT f g where
  type NTTyCxt (f :: * -> *) (g :: * -> *) a :: Exts.Constraint
  type NTCxt (f :: * -> *) (g :: * -> *) :: * -> *
  ntA :: forall a. NTTyCxt f g a => f a -> (NTCxt f g) (g a)

instance NT [] Maybe where
  type NTTyCxt [] Maybe a = ()
  type NTCxt [] Maybe = Identity
  ntA = Identity . listToMaybe

instance NT f f where
  type NTTyCxt f f a = ()
  type NTCxt f f = Identity
  ntA = Identity

instance NT [] Set where
  type NTTyCxt [] Set a = Ord a
  type NTCxt [] Set = Identity
  ntA = Identity . Set.fromList

testNT :: Set Int
testNT = runIdentity $ ntA l1

-- type class for NT-like in containers

class NT f g => GNT s t f g | s -> f, t -> g where
  gntA :: s -> (NTCxt f g) t

instance (NT f g, NTTyCxt f g a, NTTyCxt f g b) =>
         GNT (f a, f b) (g a, g b) f g where
  gntA (a, b) = (,) <$> ntA a <*> ntA b

testGNT :: (Set Int, Set Bool)
testGNT = runIdentity $ gntA p1
