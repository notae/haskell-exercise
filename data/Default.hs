-- Example of representaiton of default values

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Default where

import Data.Maybe (fromMaybe)

--- Original definition
data Color = Red | Green | Blue deriving (Show, Read, Eq, Ord)

--- with Maybe

r, g, b :: Maybe Color
[r, g, b] = map Just [Red, Green, Blue]
d :: Maybe Color
d = Nothing

cs :: [Maybe Color]
cs = [r, g, b, d]

{-|
>>> noDefault Green cs
[Red,Green,Blue,Green]
-}
noDefault :: Color -> [Maybe Color] -> [Color]
noDefault = map . fromMaybe

--- with type class and existential type

class Show c => IsColor c where
  toColor :: c -> Color

data C = forall c. IsColor c => C c
deriving instance Show C

cToColor :: C -> Color
cToColor (C c) = toColor c

instance IsColor Color where
  toColor = id

data DefaultColor = DefaultColor  deriving (Show, Read, Eq, Ord)
instance IsColor DefaultColor where
  toColor _ = Blue

data DefaultColor2 = DefaultColor2  deriving (Show, Read, Eq, Ord)
instance IsColor DefaultColor2 where
  toColor _ = Red

r', g', b', d', d2':: C
(r', g', b', d', d2') = (C Red, C Green, C Blue,
                         C DefaultColor, C DefaultColor2)

cs2 :: [C]
cs2 = [r', g', b', d', d2']

{-|
>>> noDefault' cs2
[Red,Green,Blue,Blue,Red]
-}
noDefault' :: [C] -> [Color]
noDefault' = map cToColor

--- with type class for general data

class HasDefault a where
  defaultValue :: a

instance HasDefault Color where
  defaultValue = Green

-- with Maybe

resolve :: HasDefault a => Maybe a -> a
resolve = fromMaybe defaultValue

--- with sum type

data Default a = Default | Value a deriving (Read, Show, Eq, Ord)

resolve' :: HasDefault a => Default a -> a
resolve' Default   = defaultValue
resolve' (Value a) = a

-- with default value in short symbol

__ :: HasDefault a => a
__ = defaultValue
cs3 :: [Color]
cs3 = [Red, Green, Blue, __]

-- for list type

instance HasDefault [Color] where
  defaultValue = [Red, Blue]
cs4 :: [[Color]]
cs4 = [[Red], [Blue, Green], [], __]

test :: Int -> Int
test v = 1
