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

data Rect__ p =
  Rect { rect__Begin :: p
       , rect__End   :: p }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

type Rect    = Rect__  Point
type Rect_ f = Rect__ (Point_ f)

makeFields ''Point__
makeFields ''Line__
makeFields ''CLine__
makeFields ''Rect__

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

{-|
>>> (lift :: CLine -> CLine_ []) cline1
CLine {cLine__Line = Line {line__Src = Point {point__X = [1], point__Y = [2]}, line__Dest = Point {point__X = [3], point__Y = [4]}}, cLine__Color = [Red]}
-}
line1 :: Line
line1 = Line (Point 1 1) (Point 2 3)

cline1 :: CLine
cline1 = CLine (Line (Point 1 2) (Point 3 4)) Red

cline2 :: CLine_ []
cline2 = CLine (Line (Point [1] [2]) (Point [3] [4])) [Red]

rect1 :: Rect
rect1 = Rect (Point 1 2) (Point 3 4)

rect2 :: Rect_ []
rect2 = Rect (Point [1] [2]) (Point [3] [4])
