{-
Waifu2xRepa.hs by @notae_c based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
MIT license, see https://github.com/nagadomi/waifu2x/blob/master/LICENSE
-}

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Functor.Identity
import Data.Word
import System.Environment    (getArgs)

import Control.Lens

import qualified Data.Array.Repa              as R
import qualified Data.Array.Repa.Stencil.Dim2 as R

import Data.Array.Repa ((:.) (..), All (..), Any (..), Array, D, DIM0, DIM1,
                        DIM2, DIM3, U, Z (..))
import Data.Array.Repa (Shape, Source)

import Data.Array.Repa.Repr.ForeignPtr (F)

import Codec.Picture
import Codec.Picture.Types

import qualified Codec.Picture.Repa as R

import Common
import Model

--
-- Waifu2x Core with Repa
--

waifu2x :: Model -> DynamicImage -> Either String DynamicImage
waifu2x model dimg = do
  model' <- checkModel model
  let img = R.convertImage dimg :: R.Img R.RGB
  let img' = R.onImg (waifu2xMain model') img
  return $ R.imgToImage img'

waifu2xMain :: (Source s Word8) =>
               Model -> Array s DIM3 Word8 -> Array F DIM3 Word8
waifu2xMain model src = dest where
  -- pre-process
  srcf :: Array D DIM3 Float
  srcf = R.map ((/ 255) . fromRational . toRational) src
  src2x :: Array D DIM3 Float
  src2x = doubleImageNN srcf
  Z :. _ :. _ :. k = R.extent src2x
  planes0 :: [Array U DIM2 Float]
  planes0 = map splitChs [0..k-1] where
    splitChs :: Int -> Array U DIM2 Float
    splitChs c = runIdentity $ R.computeP $ R.slice src2x (Any :. (c::Int))

  -- main process
  planesN = map (R.map (*0.8)) planes0
  -- planesN = planes0

  -- post-process
  dest = runIdentity $ R.computeP $ mergeChs $ map (f . clip) planesN where
    clip :: (Source s Float) => Array s DIM2 Float -> Array D DIM2 Word8
    clip ch = R.map (floor . (* 255) . clamp 0 1) ch
    f :: (Source s Word8) => Array s DIM2 Word8 -> Array D DIM3 Word8
    f ch = R.reshape (R.extent ch :. (1::Int)) ch
    mergeChs :: [Array D DIM3 Word8] -> Array D DIM3 Word8
    mergeChs [] = error "waifu2xMain.g: ERROR: empty list"
    mergeChs [ch] = ch
    mergeChs (ch:chs) = ch R.++ mergeChs chs

--
--
--

checkModel :: Model -> Either String Model
checkModel model = checkLength model >>= checkNOutputPlane
  where
  checkLength m =
    if l > 1
    then Right m
    else Left $ "invalid number of steps: " ++ show l
    where l = length m
  checkNOutputPlane m =
    if nop == 3
    then Right m
    else Left $ "invalid number of output planes: " ++ show nop
    where nop = last m ^. nOutputPlane
  -- TBD: more check

toImageRGB8 :: DynamicImage -> Either String (Image PixelRGB8)
toImageRGB8 dimg = case dimg of
  ImageYCbCr8 img -> Right $ convertImage img
  ImageRGB8 img -> Right img
  ImageRGBA8 img -> Right $ pixelMap dropTransparency img
  _ -> Left $ "Unsupported image type: " ++ showImageType dimg

doubleImageNN :: (Source s a) => Array s DIM3 a -> Array D DIM3 a
doubleImageNN src = R.fromFunction sh' f where
  sh@(Z :. h :. w :. k) = R.extent src
  sh' = Z :. h*2 :. w*2 :. k
  f (Z :. y :. x :. c) = src R.! (Z :. (y `div` 2) :. (x `div` 2) :. c)

--
-- Frontend
--

main :: IO ()
main = do
  [mPath, iPath, oPath] <- getArgs
  dumpTitle "Waifu2x.hs with Repa"
  dump "model path" mPath
  dump "input path" iPath
  dump "output path" oPath
  model <- readModel mPath
  dumpModel model
  Right (dimg, md) <- readImageWithMetadata iPath
  dumpImageInfo "Input Image" dimg md
  dumpTitle "Processing"
  let result = waifu2x model dimg
  case result of Left s -> putStrLn $ "ERROR: " ++ s
                 Right dimg' -> do savePngImage oPath dimg'
                                   dumpImageInfo "Output Image" dimg' md


--
-- for debug
--

test :: IO ()
test = do
  Right img <- R.readImage "test.jpg"
  let img' = R.onImg (runIdentity . R.computeP . invert) img :: R.Img R.RGB
  savePngImage "test.png" (R.imgToImage img')

invert :: (Source s a, Num a, Shape sh) => Array s sh a -> Array D sh a
invert = R.map (`subtract` 256)
