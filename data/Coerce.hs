{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coerce where

import Data.Coerce

newtype V = V Int deriving (Show, Read, Eq, Ord, Num)

test :: [[Int]]
test = coerce [[V 1, V 2], [V 3, V 4, V 5]]
