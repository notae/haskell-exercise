{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module SBVTest2 where

import Control.Arrow (first)
import Control.Monad (forM_, replicateM)
import Data.Generics
import Data.List     (sort, sortBy)
import GHC.TypeLits

import Data.SBV

import SBVExts
import SBVTest (Color (..), SColor)

{-
an example of user-defined data type
-}
data V_ i c = V { vi :: i, vc :: c } deriving (Show, Eq, Ord)
type V = V_ Integer Color
type SV = V_ SInteger SColor

instance SatModel V where
  parseCWs is = do (i, cs) <- parseCWs is
                   (c, xs) <- parseCWs cs
                   return (V i c, xs)

instance SatVar SV where
  varExists = exists_ >>= \i -> exists_ >>= \c -> return $ V i c

instance SatSpace SV where
  type Val SV = V

{-|
>>> test1
[V {vi = -1, vc = Red},V {vi = 0, vc = Green},V {vi = 1, vc = Blue},V {vi = -1, vc = Yellow},V {vi = 1, vc = Red},V {vi = -1, vc = Green},V {vi = 0, vc = Red},V {vi = 1, vc = Yellow},V {vi = 1, vc = Green},V {vi = 0, vc = Yellow},V {vi = 0, vc = Blue},V {vi = -1, vc = Blue}]
-}
test1 :: IO [V]
test1 = allSat' $ \((V i c) :: SV) -> return $ abs i .<= 1

{-|
>>> (==) <$> test1 <*> test2
True
-}
test2 :: IO [V]
test2 = allSat' $ \((V i c) :: SV) -> return $ abs i .<= 1

{-|
>>> test3
[-1,0,1]
-}
test3 :: IO [Integer]
test3 = allSat' $ \(i :: SInteger) -> return $ abs i .<= 1

{-
an example of user-defined container type
-}
newtype SizedList (l :: Nat) a =
  SizedList [a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

mkSList :: forall l a. KnownNat l => [a] -> SizedList l a
mkSList xs =
  if natVal (Proxy :: Proxy l) == l
  then SizedList xs
  else error $ "mkSList: invalid size: " ++ show l
  where l = toInteger (length xs)

lengthSList :: forall l a. KnownNat l => SizedList l a -> Integer
lengthSList _ = natVal (Proxy :: Proxy l)

instance (SatModel (Val (SizedList l v)), SatVar (SizedList l v),
          SatSpace v, KnownNat l) =>
         SatSpace (SizedList l v) where
  type Val (SizedList l v) = SizedList l (Val v)

instance (SatModel a, KnownNat l) => SatModel (SizedList l a) where
  parseCWs xs = first mkSList <$> parseCWs0 l xs
    where
      l = natVal (Proxy :: Proxy l)
      parseCWs0 :: Integer -> [CW] -> Maybe ([a], [CW])
      parseCWs0 _ [] = Just ([], [])
      parseCWs0 0 _  = Just ([], [])
      parseCWs0 l xs =
        case parseCWs xs of
         Just (a, ys) -> case parseCWs0 (l - 1) ys of
                          Just (as, zs) -> Just (a:as, zs)
                          Nothing       -> Just ([], ys)
         Nothing     -> Just ([], xs)

instance (SatVar v, KnownNat l) => SatVar (SizedList l v) where
  varExists = do
    let l = fromInteger $ natVal (Proxy :: Proxy l)
    mkSList <$> replicateM l varExists

{-|
>>> extractSLists
[SizedList [0,0,0],SizedList [0,0,1],SizedList [1,0,0],SizedList [1,0,1],SizedList [0,1,0],SizedList [0,1,1],SizedList [1,1,0],SizedList [1,1,1]]
>>> length <$> extractSLists
8
-}
extractSLists :: IO [SizedList 3 Integer]
extractSLists = allSat' $ \(SizedList xs :: SizedList 3 SInteger) -> do
  forM_ xs $ \x -> constrain $ 0 .<= x &&& x .<= 1
  return (true :: SBool)

{-|
>>> length <$> extractSLists2
144
-}
extractSLists2 :: IO [SizedList 2 V]
extractSLists2 = allSat' $ \(SizedList xs :: SizedList 2 SV) -> do
  forM_ xs $ \(V i c) -> constrain $ abs i .<= 1
  return (true :: SBool)

{-|
returned only existential variables
>>> testForAll
[0,-1,-2]
-}
testForAll :: IO [Integer]
testForAll = do
  r <- allSat $ do
    (x :: SInteger) <- exists "x"
    constrain $ -2 .<= x &&& x .<= 3
    y <- forall "y"
    return $ y * y .>= x
  return $ extractModels r

-- error "Existential arrays are not currently supported."
testForAll2 =
  allSat $ \(a :: SArray Integer Integer) -> do
    i <- forall_
    constrain $ readArray a i .== literal 1
    return (true :: SBool)
  -- return $ extractModels r

{-|
encode and decode variable length list
  with dummy values (representing empty elements)

>>> sortVList <$> testVList
[[1,4,1],[1,5,1],[1,2,5,1],[1,4,1,4,1],[1,4,1,5,1],[1,5,1,4,1],[1,5,1,5,1],[1,6,2,5,1]]
-}
testVList :: IO [[Integer]]
testVList = do
  r <- allSat pred
  return $ takeWhile (/= emptyElem) <$> extractModels r
  where
    emptyElem = 0
    pred = do
      -- encoding for variable length list
      let empty = literal emptyElem
          minLen = 3
          maxLen = 5
      (xs :: [SInteger]) <- mkExistVars (maxLen + 1)
      constrain $ last xs .== empty
      let pair = zip xs (tail xs)
      forM_ (take minLen xs) $ \x -> constrain $ x ./= empty
      forM_ pair $ \(p, n) -> constrain $ p .== empty ==> n .== empty
      -- domain specific constraints
      forM_ xs $ \x -> constrain $ x `inRange` (0, 7)
      constrain $ head xs .== 1
      forM_ pair $ \(p, n) -> do
        constrain $ p .== 1 ==> n ./= 1
        constrain $ p .== 4 ==> n .== 1
        constrain $ p .== 7 ==> n .== 3
        constrain $ p .== 3 ==> n .== 6
        constrain $ p .== 6 ==> n .== 2
        constrain $ p .== 2 ==> n .== 5
        constrain $ p .== 5 ==> n .== 1
      return (true :: SBool)

sortVList :: Ord a => [[a]] -> [[a]]
sortVList = sortBy f where
  f :: Ord a => [a] -> [a] -> Ordering
  f xs ys = case compare (length xs) (length ys) of
    EQ -> compare xs ys
    ord -> ord

instance (SatModel (Val (a, b)), SatVar (a, b)) => SatSpace (a, b) where
  type Val (a, b) = (Val a, Val b)

instance (SatVar a, SatVar b) => SatVar (a, b) where
  varExists = varExists >>= \a -> varExists >>= \b -> return (a, b)
