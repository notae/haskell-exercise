module Num where

import Data.Maybe (fromMaybe)

data Expr
  = Value Int
  | Var String
  | Expr :+ Expr
  | Expr :* Expr
  deriving (Show, Read, Eq)

instance Num Expr where
  fromInteger i = Value (fromInteger i)
  a + b = a :+ b
  a * b = a :* b

{-|
>>> eval [("x", 10), ("y", 3)] (Var "x" + Var "y" * 2)
16
-}
eval :: [(String, Int)] -> Expr -> Int
eval _ (Value i) = i
eval e (Var s) = fromMaybe 0 (lookup s e)
eval e (x :+ y) = eval e x + eval e y
eval e (x :* y) = eval e x * eval e y


ex1 :: Num a => a
ex1 = (1 + 2) * 3

{-|
>>> unS $ ex1
"(1+2)*(3)"
-}
newtype ExprS = S { unS :: String }
instance Num ExprS where
  fromInteger i = S $ show i
  S a + S b = S $ a ++ "+" ++ b
  S a * S b = S $ "(" ++ a ++ ")" ++ "*" ++ "(" ++ b ++ ")"

{-|
>>> unV $ ex1
9
-}
newtype ExprV = V { unV :: Int }
instance Num ExprV where
  fromInteger i = V $ fromInteger i
  V a + V b = V $ a + b
  V a * V b = V $ a * b
