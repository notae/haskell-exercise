{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Fop12_2 where

import Text.PrettyPrint

--
-- type representation
--

data Type t where
  RInt ∷ Type Int
  RChar ∷ Type Char
  RList ∷ Type a → Type [a]
  RPair ∷ Type a → Type b → Type (a, b)

deriving instance Show (Type t)

{-|
>>> RPair RInt (RList RChar)
RPair RInt (RList RChar)
>>> :t RPair RInt (RList RChar)
RPair RInt (RList RChar) :: Type (Int, [Char])
-}

rString ∷ Type String
rString = RList RChar

{-|
>>> RPair RInt rString
RPair RInt (RList RChar)
>>> :t RPair RInt rString
RPair RInt rString :: Type (Int, String)
-}

--
-- data serialization into list of bits
--

data Bit = B0 | B1 deriving (Show, Eq)

compress ∷ Type t → t → [Bit]
compress (RInt) i          = compressInt i
compress (RChar) c         = compressChar c
compress (RList ra) []     = B0 : []
compress (RList ra) (a:as) = B1 : compress ra a ++ compress (RList ra) as

{-|
>>> fmap compressInt [0,1,2,3]
[[B0],[B1],[B1,B0],[B1,B1]]
-}
compressInt ∷ Int → [Bit]
compressInt i = bs ++ [b] where
  i2 = i `div` 2
  bs = if i2 > 0 then compressInt i2 else []
  b  = if i `mod` 2 == 0 then B0 else B1

compressChar ∷ Char → [Bit]
compressChar c = compressInt (fromEnum c)

--
-- pretty printer
--

pretty ∷ Type t → t → Doc
pretty (RInt) i             = prettyInt i
pretty (RChar) c            = prettyChar c
pretty (RList RChar) s      = prettyString s
pretty (RList ra) []        = lbrack <> rbrack
pretty (RList ra) (a:as)    = block 1 (lbrack <> pretty ra a <> prettyL as)
  where prettyL []          = rbrack
        prettyL (a:as)      = comma <> pretty ra a <> prettyL as
pretty (RPair ra rb) (a, b) = block 1 (lparen <> pretty ra a <> comma <>
                                       pretty rb b <> rparen)

block ∷ Int → Doc → Doc
block i d = nest i d

prettyInt    = int
prettyChar   = char
prettyString = doubleQuotes . text
