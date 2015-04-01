{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Fop12_5 where

s = \x y z → (x z) (y z)
k = \ x y → x
i = \x → x

infixr :→

data Type τ where
  RBase ∷ Type Base
  (:→) ∷ Type α → Type β → Type (α → β)

b ∷ Type Base
b = RBase

data Term τ where
  App ∷ (Term (α → β)) → (Term α) → Term β
  Fun ∷ (Term α → Term β) → Term (α → β)
  Var ∷ String → Term τ

-- instance Show (Term τ) where
--   show App =

newtype Base = In { out ∷ Term Base }

reify :: ∀τ. Type τ → (τ → Term τ)
reify (RBase) v = out v
reify (ra :→ rb) v = Fun (\x → reify rb (v (reflect ra x)))

reflect :: ∀τ. Type τ → (Term τ → τ)
reflect (RBase) e = In e
reflect (ra :→ rb) e = \x → reflect rb (App e (reify ra x))
