{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Fop12_3 where

data Type t where
  RDyn ∷ Type Dynamic
  RInt ∷ Type Int
  RChar ∷ Type Char
  RList ∷ Type a → Type [a]
  RPair ∷ Type a → Type b → Type (a, b)

rString ∷ Type String
rString = RList RChar

-- for debug
instance Show (Type t) where
  show RDyn = "Dynamic"
  show RInt = "Int"
  show RChar = "Char"
  show (RList RChar) = "String"
  show (RList ra) = "[" ++ show ra ++ "]"
  show (RPair ra rb) = "(" ++ show ra ++ "," ++ show rb ++ ")"

data Dynamic = ∀ τ. Dyn (Type τ) τ

-- for debug
instance Show Dynamic where
  show (Dyn ra _) = show ra

{-|
>>> :type ds
ds :: [Dynamic]
>>> ds
[Int,String]
-}
ds :: [Dynamic]
ds = [Dyn RInt 60, Dyn rString "Bird"]

dds :: Dynamic
dds = Dyn (RList RDyn) ds
