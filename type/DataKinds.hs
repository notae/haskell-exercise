{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module DataKinds where

data Access = Public | Private deriving (Show, Eq)

data Object (n::Access) = Object Int deriving (Show, Eq)

instance Num (Object n) where
  Object x + Object y = Object $ x + y
