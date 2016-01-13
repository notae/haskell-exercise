{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module SBVTest3 where

import GHC.Exts

import Data.SBV

class Symantics repl where
  type Ctx repl a :: Constraint
  int :: Ctx repl i => i -> repl i
  add :: Ctx repl i => repl i -> repl i -> repl i
  eq  :: Eq i => repl i -> repl i -> repl Bool
  lam :: (repl a -> repl b) -> repl (a -> b)
  app :: repl (a -> b) -> repl a -> repl b

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
  type Ctx S a = Show a
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

instance Symantics R where
  type Ctx R a = Num a
  int x     = R x
  add e1 e2 = R $ unR e1 + unR e2
  eq e1 e2  = R $ unR e1 == unR e2
  lam f     = R $ unR . f . R
  app e1 e2 = R $ (unR e1) (unR e2)

-- newtype RSBV a = RSBV { unRSBV :: a }

instance Symantics SBV where
  type Ctx SBV a = (Num a, SymWord a)
  int x     = literal x
  add e1 e2 = e1 + e2
  eq e1 e2  = e1 .== e2
--   lam f     = R $ unR . f . R
--   app e1 e2 = R $ (unR e1) (unR e2)

-- predicate in tag-less final representation
p :: (Symantics repl, Ctx repl Word8) => repl Word8 -> repl Bool
p x = (int 2 `add` x) `eq` int 5

{-|
print
>>> s
"((2+3)=5)"
-}
s :: String
s = unS (p (int 3)) 0

{-|
solve as constraints
>>> r
Solution #1:
  s0 = 3 :: Word8
This is the only solution.
-}
r :: IO AllSatResult
r = allSat (p :: SWord8 -> SBool)

{-|
evaluate as Haskell expression
>>> v
True
-}
v :: Bool
v = unR $ p (int 3)
