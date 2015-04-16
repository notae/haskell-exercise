{-# LANGUAGE RankNTypes #-}

--
-- Examples for package basic-lens
--

module Basic where

import Control.Applicative (pure)
import Control.Applicative (Applicative)
import Control.Lens.Basic

infixl 0 ^.
(^.) :: s -> Lens s t a b -> a
(^.) = flip view

infixl 1 .~
(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

infixl 1 %~
(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

data Person = Person String Int deriving Show
name :: Lens Person Person String String
name f (Person a x) = fmap (\b -> Person b x) (f a)
age :: Lens Person Person Int Int
age f (Person x a) = fmap (\b -> Person x b) (f a)

data Book = Book String Person String deriving Show
title :: Lens Book Book String String
title f (Book a x y) = fmap (\b -> Book b x y) (f a)
author :: Lens Book Book Person Person
author f (Book x a y) = fmap (\b -> Book x b y) (f a)

person1 :: Person
person1 = Person "foo" 14

book1 :: Book
book1 = Book "hoge" person1 "content"

{-|
>>> book1 ^. author . age
14
>>> (title .~ "moge") book1
Book "moge" (Person "foo" 14) "content"
>>> (author . name %~ reverse) book1
Book "hoge" (Person "oof" 14) "content"
-}
