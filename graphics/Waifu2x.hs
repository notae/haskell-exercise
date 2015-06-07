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
import           Control.DeepSeq (force)
import qualified Data.ByteString.Lazy as B
import           Data.Data            (Data, Typeable, toConstr)
import           Data.Either
import           Data.List            (foldl', foldl1')
import           Debug.Trace          (trace, traceShow)
import           GHC.Generics         (Generic)
import           System.Environment   (getArgs)

import Codec.Picture
import Codec.Picture.Metadata (Metadatas)
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

type Plane = Image PixelF

type Model = [Step]

data Step =
  Step
  { _nInputPlane  :: Int
  , _nOutputPlane :: Int
  , _weight       :: [[Kernel]]   -- ^ nOutputPlane * nInputPlane * (3*3)
  , _bias         :: [Bias]      -- ^ nOutputPlane
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

--
-- Image Operations
--

toImageYCbCr8 :: DynamicImage -> Maybe (Image PixelYCbCr8)
toImageYCbCr8 dimg = case dimg of
  ImageYCbCr8 img -> Just img
  ImageRGB8 img -> Just $ convertImage img
  _ -> Nothing

doubleImageNN :: Pixel a => Image a -> Image a
doubleImageNN src = dst where
  w = imageWidth src
  h = imageHeight src
  dst = generateImage f (w * 2) (h * 2)
  f x y = pixelAt src (x `div` 2) (y `div` 2)

padEdge :: Pixel a => Int -> Image a -> Image a
padEdge n img = img' where
  (w, h) = getImageSize img
  img' = generateImage f (w + n * 2) (h + n * 2)
  f x y = pixelAt img x' y' where
    x' = clamp n (n + w - 1) x - n
    y' = clamp n (n + h - 1) y - n

{-|
>>> fmap (clamp 2 5) [0, 3, 7]
[2,3,5]
-}
clamp :: Ord a => a -> a -> a -> a
clamp l h = min h . max l

getImageSize :: Image a -> (Int, Int)
getImageSize dimg = (w, h) where
  w = imageWidth  dimg
  h = imageHeight dimg

getDynamicImageSize :: DynamicImage -> (Int, Int)
getDynamicImageSize dimg = (w, h) where
  w = dynamicMap imageWidth  dimg
  h = dynamicMap imageHeight dimg

-- plane operations
cutNeg :: Plane -> Plane
cutNeg = pixelMap $ \y -> max y 0 + 0.1 * min y 0

convolute :: Kernel -> Plane -> Plane
convolute k p = p' where
  (w, h) = getImageSize p
  (w', h') = (w - 2, h - 2)
  p' = generateImage f w' h'
  f'' x y = pixelAt p (x+1) (y+1) * 0.99
  f' x y = traceShow (x, y, f x y) (f x y)
  f x y = sum $ fmap gy (zip k [0..]) where
    gy :: ([PixelF], Int) -> PixelF
    gy (kl, ky) = sum $ fmap gx (zip3 kl (repeat ky) [0..]) where
    gx :: (PixelF, Int, Int) -> PixelF
    gx (kp, ky, kx) = pixelAt p (x+kx) (y+ky) * kp

sumP :: [Plane] -> Plane
sumP [] = error "sumP: empty list"
sumP ps@(p0:_) = s where
  (w, h) = getImageSize p0
  s = generateImage f w h
  f x y = sum (fmap (\p -> pixelAt p x y) ps)

addBias :: Float -> Plane -> Plane
addBias b = pixelMap (+b)

--
-- Waifu2x Core
--

waifu2x :: Model -> DynamicImage -> Either String DynamicImage
waifu2x model dimg = do
  img <- case toImageYCbCr8 dimg of
           Nothing -> Left $ "Unsupported image type: " ++ showImageType dimg
           Just a -> Right a
  model' <- checkModel model
  let img' =  waifu2xMain model' img
  return $ ImageYCbCr8 img'

checkModel :: Model -> Either String Model
checkModel model = checkLength model >>= checkNOutputPlane
  where
  checkLength m =
    if l > 1
    then Right m
    else Left $ "invalid number of steps: " ++ show l
    where l = length m
  checkNOutputPlane m =
    if nop == 1
    then Right m
    else Left $ "invalid number of output planes: " ++ show nop
    where nop = last m ^. nOutputPlane
  -- TBD: more check

waifu2xMain :: Model -> Image PixelYCbCr8 -> Image PixelYCbCr8
waifu2xMain model img = img' where
  -- pre-process
  img2x = doubleImageNN img
  yf :: Plane
  yf = promoteImage (extractLumaPlane img2x)
  planes0 :: [Plane]
  planes0 = [padEdge (length model) yf]
  count = sum [step ^. nInputPlane * step ^. nOutputPlane | step <- model]
  -- TBD: progress in StateT

  -- main process
  yf' :: [Plane]
  yf' = foldl' procStep planes0 (zip model [0..]) where
    procStep :: [Plane] -> (Step, Int) -> [Plane]
    procStep inPlanes (step, i) |
      trace ("procStep: " ++ show i ++ "," ++
             show (length (force (inPlanes))) ++ "," ++
             show (step ^. nInputPlane) ++ "," ++
             show (step ^. nOutputPlane) ++ "," ++
             show (length (step ^. weight)))
      False = undefined
    procStep inPlanes (step, _) =
      zipWith procOutPlane (step ^. weight) (step ^. bias) where
        procOutPlane :: [Kernel] -> Bias -> Plane
        procOutPlane ws b |
          trace ("procOutPlane: " ++ show (length ws)) False = undefined
        procOutPlane ws b = cutNeg . addBias b . sumP $
                            zipWith convolute ws inPlanes

  -- post-process
  y8' :: Image (PixelBaseComponent PixelYCbCr8)
  y8' = pixelMap (floor . (* 255) . clamp 0 1) (head yf')
  img' = pixelMapXY f img2x where
    f :: Int -> Int -> PixelYCbCr8 -> PixelYCbCr8
    f x y p = setY p (pixelAt y8' x y)
    setY :: PixelYCbCr8 -> PixelBaseComponent PixelYCbCr8 -> PixelYCbCr8
    setY (PixelYCbCr8 _ cb cr) py = PixelYCbCr8 py cb cr

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
  let (w, h) = getDynamicImageSize dimg
  dump "width" w
  dump "height" h
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
