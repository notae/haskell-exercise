{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SBVTest where

import Data.SBV
import Data.Generics
import Data.Maybe (catMaybes)

-- | Pythagorean triple
pyt :: SInteger -> (SInteger, SInteger, SInteger) -> SBool
pyt n (x, y, z) =
  x * x + y * y .== z * z &&&
  1 .<= x &&& x .<= n &&&
  1 .<= y &&& y .<= n &&&
  1 .<= z &&& z .<= n &&&
  x .<= y

{-|
>>> allSatPyt 15
Solution #1:
  s0 =  9 :: Integer
  s1 = 12 :: Integer
  s2 = 15 :: Integer
Solution #2:
  s0 =  5 :: Integer
  s1 = 12 :: Integer
  s2 = 13 :: Integer
Solution #3:
  s0 =  6 :: Integer
  s1 =  8 :: Integer
  s2 = 10 :: Integer
Solution #4:
  s0 = 3 :: Integer
  s1 = 4 :: Integer
  s2 = 5 :: Integer
Found 4 different solutions.
-}
allSatPyt = allSat . pyt

{-|
Extract ordinal values.
TBD
-}

{-|
Re-use simple functions
TBD
-}

{-|
Optimization
>>> maximize Quantified head 1 (\[x] -> simple x)
Just [3]
-}
simple :: SInteger -> SBool
simple x = x * x .< 10

{-|
Monadic style:
>>> allSat monadic
Solution #1:
  x = 2 :: Integer
  y = 1 :: Integer
Solution #2:
  x = 1 :: Integer
  y = 2 :: Integer
Found 2 different solutions.
-}
monadic :: Symbolic SBool
monadic = do x <- exists "x"
             y <- exists "y"
             constrain $ x .> 0
             constrain $ y .> 0
             constrain $ x + y .== (3 :: SInteger)
             return (true :: SBool)

{-|
Enumeration
>>> allSat enum
Solution #1:
  s0 = Red :: Color
Solution #2:
  s0 = Green :: Color
Solution #3:
  s0 = Blue :: Color
Solution #4:
  s0 = Yellow :: Color
Found 4 different solutions.
-}

data Color = Red | Green | Blue | Yellow
           deriving (Show, Read, Eq, Ord, Data, SymWord, HasKind)
type SColor = SBV Color

enum :: SColor -> Symbolic SBool
enum c = return $ c .== c

{-|
Extract enumeration values. This is *not* type-checked at compile time.
>>> extractEnum
[Red,Green,Blue,Yellow]
-}
extractEnum :: IO [Color]
extractEnum = do
  r <- allSat $ do (x :: SColor) <- exists "c"
                   return $ x .== x
  return $ catMaybes $ getModelValues "c" r
