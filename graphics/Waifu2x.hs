{-
Waifu2x.hs based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           Data.Data            (Data, Typeable, toConstr)
import           Debug.Trace          (traceShow)
import           GHC.Generics         (Generic)
import           System.Environment   (getArgs)

import Codec.Picture
import Codec.Picture.Metadata
import Codec.Picture.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

--
-- Utils
--

dump :: Show a => String -> a -> IO ()
dump name val = putStrLn $ name ++ ": " ++ show val

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
  dump "model steps" (length model)
  mapM_ dumpStep model

dumpStep :: Step -> IO ()
dumpStep step = do
  putStrLn "== Step =="
  dump "nInputPlane" (step ^. nInputPlane)
  dump "nOutputPlane" (step ^. nOutputPlane)

--
-- Core
--

waifu2x :: Model -> DynamicImage -> DynamicImage
waifu2x model = id

--
-- Frontend
--

convMain :: FilePath -> FilePath -> FilePath -> IO ()
convMain mPath iPath oPath = do
  model <- readModel mPath
  dumpModel model
  Right (dimg, md) <- readImageWithMetadata iPath
  dumpImageInfo dimg md
  let dimg' = waifu2x model dimg
  savePngImage oPath dimg'

dumpImageInfo :: DynamicImage -> Metadatas -> IO ()
dumpImageInfo dimg md = do
  putStrLn "== Input Image =="
  dump "width" (dynamicMap imageWidth dimg)
  dump "height" (dynamicMap imageHeight dimg)
  dump "image type" (showImageType dimg)
  dump "metadata" md

showImageType :: DynamicImage -> String
showImageType = f where
  f (ImageY8 _) = "ImageY8"
  f (ImageY16 _) = "ImageY16"
  f (ImageYF _) = "ImageYF"
  f (ImageYA8 _) = "ImageYA8"
  f (ImageYA16 _) = "ImageYA16"
  f (ImageRGB8 _) = "ImageRGB8"
  f (ImageRGB16 _) = "ImageRGB16"
  f (ImageRGBF _) = "ImageRGBF"
  f (ImageRGBA8 _) = "ImageRGBA8"
  f (ImageRGBA16 _) = "ImageRGBA16"
  f (ImageYCbCr8 _) = "ImageYCbCr8"
  f (ImageCMYK8 _) = "ImageCMYK8"
  f (ImageCMYK16 _) = "ImageCMYK16"

-- deriving instance Generic DynamicImage

-- deriving instance Typeable DynamicImage
-- deriving instance Data DynamicImage

-- showDynamicImageType :: DynamicImage -> String
-- showDynamicImageType = conName

-- conName :: G.Constructor c => G.M1 i c f p -> String
-- conName m1 = G.conName m1

conName :: Data a  => a -> String
conName = show . toConstr

main :: IO ()
main = do
  [mPath, iPath, oPath] <- getArgs
  putStrLn "== Paths =="
  dump "model path" mPath
  dump "input path" iPath
  dump "output path" oPath
  convMain mPath iPath oPath
