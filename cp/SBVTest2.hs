{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds #-}

module SBVTest2 where

import Data.Generics
import GHC.TypeLits

import Data.SBV

import SBVExts
import SBVTest (Color (..), SColor)

{-
an example of user-defined data type
-}
data V_ i c = V { vi :: i, vc :: c } deriving (Show, Eq)
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
test2 = spc allSat $ \((V i c) :: SV) -> return $ abs i .<= 1

{-|
>>> test3
[-1,0,1]
-}
test3 :: IO [Integer]
test3 = spc allSat $ \(i :: SInteger) -> return $ abs i .<= 1

{-
another example of user-defined data type
-}
data SizedList (l :: Nat) a = SizedList [a] deriving (Show)

mkSList :: forall l a. KnownNat l => [a] -> SizedList l a
mkSList xs =
  if natVal (Proxy :: Proxy l) == l
  then SizedList xs
  else error $ "mkSList: invalid size: " ++ show l
  where l = toInteger (length xs)

lengthSList :: forall l a. KnownNat l => SizedList l a -> Integer
lengthSList _ = natVal (Proxy :: Proxy l)

instance (SatModel a, SymWord a, KnownNat l) =>
         SatSpace (SizedList l (SBV a)) where
  type Val (SizedList l (SBV a)) = SizedList l a

instance (SatModel a, KnownNat l) => SatModel (SizedList l a) where
  parseCWs [] = Just (mkSList [], [])
  parseCWs xs = case parseCWs xs of
                  Just (a, ys) -> case parseCWs ys of
                                    Just (as, zs) -> Just (mkSList (a:as), zs)
                                    Nothing       -> Just (mkSList [], ys)
                  Nothing     -> Just (mkSList [], xs)

instance (SymWord a, KnownNat l) => SatVar (SizedList l (SBV a)) where
  varExists = do
    let l = fromInteger $ natVal (Proxy :: Proxy l)
    xs <- mkExistVars l
    return $ mkSList xs

{-|
>>> extractSLists
[SizedList [0,0,0],SizedList [0,0,1],SizedList [1,0,0],SizedList [1,0,1],SizedList [0,1,0],SizedList [0,1,1],SizedList [1,1,0],SizedList [1,1,1]]
>>> length <$> extractSLists
8
-}
extractSLists :: IO [SizedList 3 Integer]
extractSLists = spc allSat $ \(SizedList xs :: SizedList 3 SInteger) -> do
  flip mapM_ xs $ \x -> constrain $ 0 .<= x &&& x .<= 1
  return $ (true :: SBool)
