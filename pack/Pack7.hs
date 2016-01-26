{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Pack7 where

import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Exts (Constraint)

-- type class for abstract sets

class ISet s where
  type ISetCxt (s :: * -> *) a :: Constraint
  fromList :: ISetCxt s a => [a] -> s a

instance ISet [] where
  type ISetCxt [] a = ()
  fromList = id

instance ISet Set where
  type ISetCxt Set a = Ord a
  fromList = Set.fromList

-- type class for mapping sets in containers

class ISet g => SetMap s t g | t -> g where
  setMap :: s -> t

instance (ISet g, ISetCxt g a, ISetCxt g b) =>
         SetMap ([a], [b]) (g a, g b) g where
  setMap (a, b) = (fromList a, fromList b)

p1 :: ([Int], [Bool])
p1 = ([1, 2], [True, False])

{-|
>>> testSetMap
(fromList [1,2],fromList [False,True])
-}
testSetMap :: (Set Int, Set Bool)
testSetMap = setMap p1

-- TBD: replace ISet to a type variable

