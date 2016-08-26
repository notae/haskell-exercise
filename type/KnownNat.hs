{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module KnownNat where

import Data.Proxy
import GHC.TypeLits

{- $setup
>>> :set -XDataKinds
-}

{-|
>>> f (Proxy :: Proxy 1)
3
-}
f :: forall n m. (KnownNat n, KnownNat m, m ~ (n + 2)) => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy m)
