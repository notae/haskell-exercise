{-
Waifu2x.hs based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- module Waifu2x where
module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           Debug.Trace          (traceShow)
import           GHC.Generics
import           System.Environment   (getArgs)
-- import           GHC.Exts             (IsList, IsString, fromString, toList)

import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Data.Aeson

main :: IO ()
main = do
  [mPath, iPath, oPath] <- getArgs
  putStrLn $ "model path: " ++ mPath
  putStrLn $ "input path: " ++ iPath
  putStrLn $ "output path: " ++ oPath
  waifu2x mPath iPath oPath

waifu2x :: FilePath -> FilePath -> FilePath -> IO ()
waifu2x mPath iPath oPath = do
  model <- readModel mPath
  dumpModel model

scale2_model_path :: FilePath
scale2_model_path = "models/scale2.0x_model.json"

readModel :: FilePath -> IO Model
readModel path = do
  json_bytes <- B.readFile path
  let (Just model) = decode' json_bytes
  return model

dumpModel :: Model -> IO ()
dumpModel model = do
  putStrLn $ "model steps: " ++ show (length model)
  mapM_ dumpStep model

dumpStep :: Step -> IO ()
dumpStep step = do
  putStrLn "== Step =="
  putStrLn $ "nInputPlane: " ++ show (nInputPlane step)
  putStrLn $ "nOutputPlane: " ++ show (nOutputPlane step)

type Model = [Step]

data Step =
  Step
  { nInputPlane  :: Int
  , nOutputPlane :: Int
  , weight       :: [[Kernel]]   -- ^ nOutputPlane * nInputPlane * (3*3)
  , bias         :: [Float]      -- ^ nOutputPlane
  , kW           :: Int
  , kH           :: Int
  } deriving (Show, Generic)

instance FromJSON Step
instance ToJSON Step

type Kernel = [[Float]]
