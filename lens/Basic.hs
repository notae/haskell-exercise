{-# LANGUAGE RankNTypes #-}

--
-- Examples for package basic-lens
--

module Basic where

import Control.Applicative (pure)
import Control.Applicative (Applicative)
import Control.Lens.Basic

infixl 0 ^.

(^.) :: Lens s t a b -> s -> a
(^.) = view

data Person = Person String Int deriving Show
_age :: Lens Person Person Int Int
_age f (Person x a) = fmap (\b -> Person x b) (f a)

data Book = Book String Person String deriving Show
_title :: Lens Book Book String String
_title f (Book a x y) = fmap (\b -> Book b x y) (f a)

person :: Person
person = Person "foo" 14

book :: Book
book = Book "hoge" person "content"

{-|
>>> test1
Book "moge" (Person "foo" 14) "content"
-}
test1 :: Book
test1 = set _title "moge" book

data P a = P String a deriving Show
_val :: Lens (P s) (P t) s t
_val f (P k v) = fmap (\b -> P k b) (f v)

liftPV :: Applicative f => P a -> P (f a)
liftPV = over _val pure

-- liftL :: Applicative f => Lens s t a b -> s -> t
-- liftL l = over l pure
