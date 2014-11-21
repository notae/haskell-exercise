module Num where

import Data.Maybe (fromMaybe)

data Expr
  = Value Int
  | Var String
  | Expr :+ Expr
  | Expr :* Expr
  deriving (Show, Read, Eq)

instance Num Expr where
  a + b = a :+ b
  a * b = a :* b
  fromInteger i = Value (fromInteger i)

{-|
>>> eval [("x", 10), ("y", 3)] (Var "x" + Var "y" * 2)
16
-}
eval :: [(String, Int)] -> Expr -> Int
eval _ (Value i) = i
eval e (Var s) = fromMaybe 0 (lookup s e)
eval e (x :+ y) = eval e x + eval e y
eval e (x :* y) = eval e x * eval e y
