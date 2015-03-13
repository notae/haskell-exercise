{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Fop12_3 where

import Control.Monad    (liftM, liftM2)
import Text.PrettyPrint

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

instance Show Dynamic where
  show = render . pretty RDyn

{-|
>>> :type ds
ds :: [Dynamic]
>>> ds
[Dynamic Int 60,Dynamic String "Bird"]
-}
ds ∷ [Dynamic]
ds = [Dyn RInt 60, Dyn rString "Bird"]

{-|
>>> dds
Dynamic [Dynamic] [Dynamic Int 60,Dynamic String "Bird"]
-}
dds ∷ Dynamic
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

pretty ∷ Type t → t → Doc
pretty (RDyn) (Dyn ra a)    = text "Dynamic" <> space <> text (show ra) <> space <> pretty ra a
pretty (RInt) i             = prettyInt i
pretty (RChar) c            = prettyChar c
pretty (RList RChar) s      = prettyString s
pretty (RList _) []         = lbrack <> rbrack
pretty (RList ra) (a:as)    = block 1 (lbrack <> pretty ra a <> prettyL as)
  where prettyL []          = rbrack
        prettyL (a:as)      = comma <> pretty ra a <> prettyL as
pretty (RPair ra rb) (a, b) = block 1 (lparen <> pretty ra a <> comma <>
                                       pretty rb b <> rparen)

block ∷ Int → Doc → Doc
block i d = nest i d

prettyInt ∷ Int → Doc
prettyInt    = int
prettyChar ∷ Char → Doc
prettyChar   = char
prettyString ∷ String → Doc
prettyString = doubleQuotes . text
