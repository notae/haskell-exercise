{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Fop12_4 where

data Type τ where
  RInt ∷ Type Int
  RChar ∷ Type Char
  RList ∷ Type a → Type [a]
  RPair ∷ Type a → Type b → Type (a, b)
  RPerson ∷ Type Person

deriving instance Show (Type τ)

rString ∷ Type String
rString = RList RChar

type Name   = String
type Age    = Int
data Person = Person Name Age

ps :: [Person]
ps = [Person "Norma" 50, Person "Richard" 59]

tick ∷ Name → Traversal
tick s (RPerson) (Person n a)
  | s == n = Person n (a + 1)
tick _ _ t = t

type Traversal = ∀τ. Type τ → τ → τ

copy ∷ Traversal
copy _ = id

(∘) :: Traversal → Traversal → Traversal
(f ∘ g) rt = f rt . g rt

imap ∷ Traversal → Traversal
imap _ (RInt) i               = i
imap _ (RChar) c              = c
imap _ (RList _) []           = []
imap f (RList ra) (a:as)      = f ra a : imap f (RList ra) as
imap f (RPair ra rb) (a, b)   = (f ra a, f rb b)
imap f (RPerson) (Person n a) = Person (f rString n) (f RInt a)

everywhere, everywhere' ∷ Traversal → Traversal
everywhere f = f ∘ imap (everywhere f)
everywhere' f = imap (everywhere' f) ∘ f

type Query θ = ∀τ. Type τ → τ -> θ

isum ∷ Query Int → Query Int
isum _ (RInt) _               = 0
isum _ (RChar) _              = 0
isum _ (RList _) []           = 0
isum f (RList ra) (a:as)      = f ra a + isum f (RList ra) as
isum f (RPair ra rb) (a, b)   = f ra a + f rb b
isum f (RPerson) (Person n a) = f rString n + f RInt a

total ∷ Query Int → Query Int
total  f rt t = f rt t + isum (total f) rt t

{-|
>>> total age (RList RPerson) ps
109
-}
age ∷ Query Int
age (RPerson) (Person _ a) = a
age _ _                    = 0

{-|
>>> total sizeof (RList RPerson) ps
43
-}
sizeof ∷ Query Int
sizeof (RInt) _        = 2
sizeof (RChar) _       = 2
sizeof (RList _) []    = 0
sizeof (RList _) (_:_) = 3
sizeof (RPair _ _) _   = 3
sizeof (RPerson) _     = 3
