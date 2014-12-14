{-# LANGUAGE RecordWildCards #-}

module RecordWildCards where

data D = D { dFirst :: Int, dSecond :: Bool } deriving (Show, Eq)

{-|
>>> deconstruct (D 123 True)
(123,True)
-}
deconstruct :: D -> (Int, Bool)
deconstruct D{..} = (dFirst, dSecond)

{-|
>>> construct (123,True)
D {dFirst = 123, dSecond = True}
-}
construct :: (Int, Bool) -> D
construct (i, b) = D{..} where
  dFirst = i
  dSecond = b
