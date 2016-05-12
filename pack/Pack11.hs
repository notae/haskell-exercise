{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Pack11 where

import Control.Lens
import Data.Functor.Compose
import Data.Functor.Product

data Point__ a =
  Point { point__X :: a
        , point__Y :: a }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

type Point    = Point__    Int
type Point_ f = Point__ (f Int)

data Line__ p =
  Line { line__Src  :: p
       , line__Dest :: p }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

type Line    = Line__  Point
type Line_ f = Line__ (Point_ f)

data Color = Red | Green | Blue deriving (Show, Read, Eq, Enum)

data CLine__ l c =
  CLine { cLine__Line  :: l
        , cLine__Color :: c }
  deriving (Show, Read, Eq)

type CLine    = CLine__  Line        Color
type CLine_ f = CLine__ (Line_ f) (f Color)

instance Bifunctor CLine__ where
  bimap f g (CLine l c) = CLine (f l) (g c)

makeFields ''Point__
makeFields ''Line__
makeFields ''CLine__

liftPoint :: Applicative f => Point -> Point_ f
liftPoint = runIdentity . traverse (Identity . pure)

liftLine :: Applicative f => Line -> Line_ f
liftLine = getCompose . runIdentity . traverse (Identity . pure) . Compose

liftCLine :: Applicative f => CLine -> CLine_ f
liftCLine = bimap liftLine pure

class Lift a a' where
  lift :: a -> a'

instance Applicative f => Lift Point (Point_ f) where
  lift = liftPoint

instance Applicative f => Lift CLine (CLine_ f) where
  lift = liftCLine

line1 :: Line
line1 = Line (Point 1 1) (Point 2 3)

{-|
>>> lift cline1 == cline2
True
-}
cline1 :: CLine
cline1 = CLine (Line (Point 1 2) (Point 3 4)) Red

cline2 :: CLine_ []
cline2 = CLine (Line (Point [1] [2]) (Point [3] [4])) [Red]
