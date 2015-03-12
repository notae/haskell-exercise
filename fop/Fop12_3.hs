{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Fop12_3 where

import Control.Monad (liftM, liftM2)

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

-- show only type for debug
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

teq ∷ ∀τ σ . Type τ → Type σ → Maybe (τ → σ)
teq (RInt) (RInt) = return id
teq (RChar) (RChar) = return id
teq (RList ra1) (RList ra2) = liftM fmap (teq ra1 ra2)
teq (RPair ra1 rb1) (RPair ra2 rb2) = liftM2 (><) (teq ra1 ra2) (teq rb1 rb2)
  where (><) f g (a, b) = (f a, g b)
teq _ _ = Nothing

{-|
>>> cast (Dyn RInt 123) RInt
Just 123
>>> cast (Dyn RInt 123) RChar
Nothing
>>> cast (Dyn rString "foo") rString
Just "foo"
>>> cast (Dyn (RPair RInt rString) (123,"foo")) (RPair RInt rString)
Just (123,"foo")
-}
cast ∷ ∀τ. Dynamic → Type τ → Maybe τ
cast (Dyn ra a) rt = fmap (\f → f a) (teq ra rt)
