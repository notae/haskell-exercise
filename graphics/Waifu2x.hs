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
import           Data.Either
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

dumpTitle :: String -> IO ()
dumpTitle name = putStrLn $ "== " ++ name ++ " =="

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
  dumpTitle "Step"
  dump "nInputPlane" (step ^. nInputPlane)
  dump "nOutputPlane" (step ^. nOutputPlane)

--
-- Image
--

toImageYCbCr8 :: DynamicImage -> Maybe (Image PixelYCbCr8)
toImageYCbCr8 dimg = case dimg of
  ImageYCbCr8 img -> Just img
  _ -> Nothing

doubleImageNN :: Pixel a => Image a -> Image a
doubleImageNN src = dst where
  w = imageWidth src
  h = imageHeight src
  dst = generateImage f (w * 2) (h * 2)
  f x y = pixelAt src (x `div` 2) (y `div` 2)

--
-- Waifu2x Core
--

waifu2x :: Model -> DynamicImage -> Either String DynamicImage
waifu2x model dimg = do
  img <- case toImageYCbCr8 dimg of
           Nothing -> Left $ "Unsupported image type: " ++ showImageType dimg
           Just a -> Right a
  model' <- validateModel model
  let img' =  waifu2xMain model' img
  return $ ImageYCbCr8 img'

validateModel :: Model -> Either String Model
validateModel model =
  if length model > 1
  then Right model
  else Left $ "invalid number of steps: " ++ show (length model)

waifu2xMain :: Model -> Image PixelYCbCr8 -> Image PixelYCbCr8
waifu2xMain model img = img' where
  img' = doubleImageNN img

--
-- Frontend
--

convMain :: FilePath -> FilePath -> FilePath -> IO ()
convMain mPath iPath oPath = do
  model <- readModel mPath
  dumpModel model
  Right (dimg, md) <- readImageWithMetadata iPath
  dumpImageInfo "Input Image" dimg md
  dumpTitle "Processing"
  let result = waifu2x model dimg
  case result of Left s -> putStrLn $ "ERROR: " ++ s
                 Right dimg' -> do savePngImage oPath dimg'
                                   dumpImageInfo "Output Image" dimg' md

dumpImageInfo :: String -> DynamicImage -> Metadatas -> IO ()
dumpImageInfo title dimg md = do
  dumpTitle title
  dump "width" (dynamicMap imageWidth dimg)
  dump "height" (dynamicMap imageHeight dimg)
  dump "image type" (showImageType dimg)
  dump "metadata" md

showImageType :: DynamicImage -> String
showImageType = f where
  f (ImageY8 _) = "Y8"
  f (ImageY16 _) = "Y16"
  f (ImageYF _) = "YF"
  f (ImageYA8 _) = "YA8"
  f (ImageYA16 _) = "YA16"
  f (ImageRGB8 _) = "RGB8"
  f (ImageRGB16 _) = "RGB16"
  f (ImageRGBF _) = "RGBF"
  f (ImageRGBA8 _) = "RGBA8"
  f (ImageRGBA16 _) = "RGBA16"
  f (ImageYCbCr8 _) = "YCbCr8"
  f (ImageCMYK8 _) = "CMYK8"
  f (ImageCMYK16 _) = "CMYK16"

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
  dumpTitle "Paths"
  dump "model path" mPath
  dump "input path" iPath
  dump "output path" oPath
  convMain mPath iPath oPath
