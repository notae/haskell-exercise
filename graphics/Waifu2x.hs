{-
Waifu2x.hs by @notae_c based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
MIT license, see https://github.com/nagadomi/waifu2x/blob/master/LICENSE
-}

module Main where

import           Control.DeepSeq    (force)
import qualified Data.Array.IArray  as A
import qualified Data.Array.Unboxed as A
import           Data.List          (foldl')
import           Debug.Trace        (trace)
import           System.Environment (getArgs)

import Codec.Picture
import Codec.Picture.Types

import Control.Lens

import Common
import Model

--
-- Image Operations
--

type Plane = Image PixelF

toImageYCbCr8 :: DynamicImage -> Maybe (Image PixelYCbCr8)
toImageYCbCr8 dimg = case dimg of
  ImageYCbCr8 img -> Just img
  ImageRGB8 img -> Just $ convertImage img
  ImageRGBA8 img -> Just $ convertImage $ pixelMap dropTransparency img
  _ -> Nothing

doubleImageNN :: Pixel a => Image a -> Image a
doubleImageNN src = generateImage f (w * 2) (h * 2) where
  (w, h) = getImageSize src
  f x y = pixelAt src (x `div` 2) (y `div` 2)

padEdge :: Pixel a => Int -> Image a -> Image a
padEdge n img = generateImage f (w + n * 2) (h + n * 2) where
  (w, h) = getImageSize img
  f x y = pixelAt img x' y' where
    x' = clamp n (n + w - 1) x - n
    y' = clamp n (n + h - 1) y - n

cutNeg :: Plane -> Plane
cutNeg = pixelMap $ \y -> max y 0 + 0.1 * min y 0

convolve :: Kernel -> Plane -> Plane
convolve k p = generateImage f w' h' where
  (w, h) = getImageSize p
  (w', h') = (w - 2, h - 2)
  (kw, kh) = (length (head k), length k)
  ary :: A.UArray (Int, Int) Float
  ary = A.listArray ((0, 0), (kw-1, kh-1)) (concat k)
  f x y = sum $ fmap (g x y) (A.assocs ary)
  g x y ((kx, ky), e) = pixelAt p (x+kx) (y+ky) * e

sumP :: [Plane] -> Plane
sumP ps = generateImage f w h where
  (w, h) = getImageSize (head ps)
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
  yf = promoteImage (extractLumaPlane img2x)
  planes0 = [padEdge (length model) yf]
  -- count = sum [step ^. nInputPlane * step ^. nOutputPlane | step <- model]
  -- TBD: progress in StateT

  -- main process
  yf' = foldl' procStep planes0 (zip model [0..]) where
    procStep :: [Plane] -> (Step, Int) -> [Plane]
    procStep inPlanes (step, i) | traceStep inPlanes (step, i) = undefined
    procStep inPlanes (step, _) =
      zipWith3 procOutPlane (step ^. weight) (step ^. bias) [0..] where
        procOutPlane :: [Kernel] -> Float -> Int -> Plane
        procOutPlane _ _ j | traceOutPlane step j = undefined
        procOutPlane ws b _ = cutNeg . addBias b . sumP $
                              zipWith convolve ws inPlanes

  -- post-process
  y8' = pixelMap (floor . (* 255) . clamp 0 1) (head yf')
  img' = pixelMapXY f img2x where
    f x y p = setY p (pixelAt y8' x y)
    setY (PixelYCbCr8 _ cb cr) py = PixelYCbCr8 py cb cr

  -- trace output
  traceStep inPlanes (step, i) =
    trace ("procStep: " ++ show i ++ "," ++
           show (length (force inPlanes)) ++ "," ++
           show (step ^. nInputPlane) ++ "," ++
           show (step ^. nOutputPlane) ++ "," ++
           show (length (step ^. weight)))
          False
  traceOutPlane step j =
    trace ("procOutPlane: " ++ show j ++ "/" ++ show (step ^. nOutputPlane)) False

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

main :: IO ()
main = do
  [mPath, iPath, oPath] <- getArgs
  dumpTitle "Paths"
  dump "model path" mPath
  dump "input path" iPath
  dump "output path" oPath
  convMain mPath iPath oPath
