{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SBVTest where

import Data.Generics
import Data.SBV

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
>>> allSat enums
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
           deriving (Show, Read, Eq, Ord, Data, SymWord, HasKind, SatModel)
type SColor = SBV Color

enums :: SColor -> SBool
enums c = c .== c

{-|
Extract enumeration values. These are *not* type-checked at compile time.
>>> extractEnums
[Red,Green,Blue,Yellow]
-}
extractEnums :: IO [Color]
extractEnums = do
  r <- allSat enums
  return $ extractModels r

{-|
Extract tuple values. These are *not* type-checked at compile time.
>>> extractTuples
[(-1,Red),(0,Green),(1,Blue),(-1,Yellow),(1,Red),(-1,Green),(0,Red),(1,Yellow),(1,Green),(0,Yellow),(0,Blue),(-1,Blue)]
-}
extractTuples :: IO [(Integer, Color)]
extractTuples = do
  r <- allSat $ \(x::SInteger, c::SColor) -> abs x .<= 1
  return $ extractModels r

{-|
Solve and extract lists.
-}
extractLists :: IO [[Integer]]
extractLists = do
  r <- allSat $ do
       vs <- mkExistVars 3
       flip mapM_ vs $ \v -> constrain $ v .>= 1 &&& v .<= 3
       constrain $ allDifferent (vs :: [SInteger])
       return $ (true :: SBool)
  return $ extractModels r
