{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import qualified Data.ByteString.Lazy as B
import           GHC.Generics         (Generic)

import Control.Lens
import Data.Aeson
import Data.Aeson.TH

import Common

--
-- Model
--

type Model = [Step]

data Step =
  Step
  { _nInputPlane  :: Int
  , _nOutputPlane :: Int
  , _weight       :: [[Kernel]] -- ^ nOutputPlane * nInputPlane * (kW*kH)
  , _bias         :: [Bias]     -- ^ nOutputPlane
  , _kW           :: Int
  , _kH           :: Int
  } deriving (Show, Generic)

type Kernel = [[Float]]
type Bias = Float

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Step

makeLenses ''Step

readModel :: FilePath -> IO Model
readModel path = do
  json_bytes <- B.readFile path
  let (Just model) = decode' json_bytes
  return model

dumpModel :: Model -> IO ()
dumpModel model = do
  dump "model steps" (length model)
  mapM_ dumpStep model

dumpStep :: Step -> IO ()
dumpStep step = do
  dumpTitle "Step"
  dump "nInputPlane" (step ^. nInputPlane)
  dump "nOutputPlane" (step ^. nOutputPlane)
  dump "kW, kH" (step ^. kW, step ^. kH)
