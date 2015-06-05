{-
Waifu2x.hs based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           Debug.Trace          (traceShow)
import           GHC.Generics
import           System.Environment   (getArgs)

import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

--
-- Model
--

type Model = [Step]

data Step =
  Step
  { _nInputPlane  :: Int
  , _nOutputPlane :: Int
  , _weight       :: [[Kernel]]   -- ^ nOutputPlane * nInputPlane * (3*3)
  , _bias         :: [Float]      -- ^ nOutputPlane
  , _kW           :: Int
  , _kH           :: Int
  } deriving (Show, Generic)

type Kernel = [[Float]]

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Step

makeLenses ''Step

readModel :: FilePath -> IO Model
readModel path = do
  json_bytes <- B.readFile path
  let (Just model) = decode' json_bytes
  return model

dumpModel :: Model -> IO ()
dumpModel model = do
  putStrLn $ "model steps: " ++ (length model & show)
  mapM_ dumpStep model

dumpStep :: Step -> IO ()
dumpStep step = do
  putStrLn "== Step =="
  putStrLn $ "nInputPlane: " ++ (step ^. nInputPlane & show)
  putStrLn $ "nOutputPlane: " ++ (step ^. nOutputPlane & show)

--
-- Core
--

waifu2x :: FilePath -> FilePath -> FilePath -> IO ()
waifu2x mPath iPath oPath = do
  model <- readModel mPath
  dumpModel model

--
-- Frontend
--

main :: IO ()
main = do
  [mPath, iPath, oPath] <- getArgs
  putStrLn $ "model path: " ++ mPath
  putStrLn $ "input path: " ++ iPath
  putStrLn $ "output path: " ++ oPath
  waifu2x mPath iPath oPath
