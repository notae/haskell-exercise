-- Example of representaiton of default values

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

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
