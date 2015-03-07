{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fop12_1 where

data Term t where
  Zero :: Term Int
  Succ :: Term Int -> Term Int
  Pred :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a

deriving instance Show (Term t)

eval :: Term t -> t
eval Zero = 0
eval (Succ e) = eval e + 1
eval (Pred e) = eval e - 1
eval (IsZero e) = eval e == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2

{-|
>>> eval $ If (IsZero (Succ Zero)) Zero (Pred Zero)
-1
-}

{-|
>>> eval (If true true false)
True
-}
zero, one :: Term Int
zero = Zero
one = Succ Zero
true, false :: Term Bool
true = IsZero Zero
false = IsZero one
