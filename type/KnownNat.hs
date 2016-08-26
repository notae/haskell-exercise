{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module KnownNat where

import Data.Proxy
import GHC.TypeLits

f :: forall n m. (KnownNat n, KnownNat m, m ~ (n + 2)) => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) +
      natVal (Proxy :: Proxy m)
