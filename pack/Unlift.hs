{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

--
-- This code come originally from:
-- [Haskell-cafe] Generalizing "unlift" functions with	monad-control
-- http://mail.haskell.org/pipermail/haskell-cafe/2015-March/118872.html
--

module Unlift where

import Control.Monad.Trans.Control (MonadTransControl (liftWith), StT)
import Control.Monad.Trans.Reader  (ReaderT)
import Data.Constraint             ((:-), (\\))
import Data.Constraint.Forall      (Forall, inst)

class    (StT t a ~ a) => Identical t a
instance (StT t a ~ a) => Identical t a

type Unliftable t = Forall (Identical t)

newtype Unlift t = Unlift { unlift :: forall n b. Monad n => t n b -> n b  }

mkUnlift :: forall t m a . (Forall (Identical t), Monad m)
        => (forall n b. Monad n => t n b -> n (StT t b)) -> t m a -> m a
mkUnlift r act = r act \\ (inst :: Forall (Identical t) :- Identical t a)

askRunG :: forall t m. (MonadTransControl t, Unliftable t, Monad m) => t m (Unlift t)
askRunG = liftWith unlifter
 where
   unlifter :: (forall n b. Monad n => t n b -> n (StT t b)) -> m (Unlift t)
   unlifter r = return $ Unlift (mkUnlift r)

askRun :: Monad m => ReaderT a m (Unlift (ReaderT a))
askRun = askRunG
