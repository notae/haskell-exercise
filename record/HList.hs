{-
Heterogeneous List Example
  from
    Strongly Typed Heterogeneous Collections
    http://okmij.org/ftp/Haskell/HList-ext.pdf
  (This HList definition is different from the version in Hackage)
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


data HNil = HNil deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)

infixr :*:
type e :*: l = HCons e l -- type level constructor
infixr .*.
-- (.*.) :: e -> l -> HCons e l
-- e .*. l = HCons e l      -- value level constructor
(.*.) :: HList l => e -> l -> HCons e l
(.*.) = HCons

class HList l
instance HList HNil
instance HList l => HList (HCons e l)

class HAppend l l' l'' | l l' -> l'' where
  hAppend :: l -> l' -> l''

{-|
>>> hAppend (1 .*. True .*. HNil) (False .*. "foo" .*. HNil)
HCons 1 (HCons True (HCons False (HCons "foo" HNil)))
>>> :t hAppend (1 .*. True .*. HNil) (False .*. "foo" .*. HNil)
hAppend (1 .*. True .*. HNil) (False .*. "foo" .*. HNil)
  :: Num e => HCons e (HCons Bool (HCons Bool (HCons [Char] HNil)))
-}
instance HList l => HAppend HNil l l where
  hAppend HNil = id
instance (HList l, HAppend l l' l'') =>
         HAppend (HCons x l) l' (HCons x l'') where
           hAppend (HCons x l) = HCons x . hAppend l

-- more example
newtype Key   = Key Integer deriving (Show,Eq,Ord)
newtype Name  = Name String deriving (Show,Eq)
data Breed    = Cow | Sheep deriving (Show,Eq)
newtype Price = Price Float deriving (Show,Eq,Ord)
data Disease  = BSE | FM    deriving (Show,Eq)

type Animal = Key :*: Name :*: Breed :*: Price :*: HNil

angus :: Animal -- optional type declaration
angus = Key 42
        .*. Name "Angus"
        .*. Cow
        .*. Price 75.5
        .*. HNil
