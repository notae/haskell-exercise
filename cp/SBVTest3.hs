{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SBVTest3 where

import Data.SBV

class Symantics repl i where
  int :: i -> repl i
  add :: Num i => repl i -> repl i -> repl i
  eq  :: Eq i => repl i -> repl i -> repl Bool
  lam :: (repl a -> repl b) -> repl (a -> b)
  app :: repl (a -> b) -> repl a -> repl b

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Show i => Symantics S i where
    int x     = S $ const $ show x
    add e1 e2 = S $ \h ->
      "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"
    eq e1 e2  = S $ \h ->
      "(" ++ unS e1 h ++ "=" ++ unS e2 h ++ ")"

    lam e = S $ \h ->
       let x = "x" ++ show h
       in "(\\" ++ x ++ " -> " ++
            unS (e (S $ const x)) (succ h) ++ ")"
    app e1 e2 = S $ \h ->
      "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

newtype R a = R { unR :: a }

instance Symantics R i where
  int x     = R x
  add e1 e2 = R $ unR e1 + unR e2
  eq e1 e2  = R $ unR e1 == unR e2
  lam f     = R $ unR . f . R
  app e1 e2 = R $ (unR e1) (unR e2)

-- newtype RSBV a = RSBV { unRSBV :: a }

instance SymWord i => Symantics SBV i where
  int x     = literal x
  add e1 e2 = e1 + e2
  eq e1 e2  = e1 .== e2
--   lam f     = R $ unR . f . R
--   app e1 e2 = R $ (unR e1) (unR e2)

-- predicate in tag-less final representation
p :: Symantics repl Word8 => repl Word8 -> repl Bool
p x = (int 2 `add` x) `eq` int 5

-- print
s :: String
s = unS (p (int 3)) 0

-- solve as constraints
r :: IO AllSatResult
r = allSat (p :: SWord8 -> SBool)

-- evaluate as Haskell expression
v :: Bool
v = unR $ p (int 3)
