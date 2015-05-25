{-# LANGUAGE FlexibleContexts #-}

module Graphics where

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Data.Maybe
import Debug.Trace         (traceShow)

-- example
imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 x' y' 128
           where x' = fromInteger (toInteger x)
                 y' = fromInteger (toInteger y)

doubleImageFileSimple :: String -> IO ()
doubleImageFileSimple path = do
  Right dimg <- readImage path
  let img' = dynamicPixelMap doubleImageSimple dimg
  let path' = "2xsimple_" ++ path
  savePngImage path' img'

doubleImageSimple :: Pixel a => Image a -> Image a
doubleImageSimple src = dst where
  w = imageWidth src
  h = imageHeight src
  dst = generateImage f (w * 2) (h * 2)
  f x y = pixelAt src (x `div` 2) (y `div` 2)

doubleImageFileAve :: String -> IO ()
doubleImageFileAve path = do
  Right dimg <- readImage path
  let img = convToImageRGB16 dimg
  let img' = ImageRGB16 $ doubleImageAve img
  let path' = "2xave_" ++ path
  savePngImage path' img'

convToImageRGB16 :: DynamicImage -> Image PixelRGB16
convToImageRGB16 dimg = case dimg of
  ImageY8 _ -> error "ImageY8"
  ImageY16 _ -> error "ImageY16"
  ImageYF _ -> error "ImageYF"
  ImageYA8 _ -> error "ImageYA8"
  ImageYA16 _ -> error "ImageYA16"
  ImageRGB8 img -> promoteImage img
  ImageRGB16 img -> img
  ImageRGBF _ -> error "ImageRGBF"
  ImageRGBA8 img -> pixelMap dropTransparency (promoteImage img :: Image PixelRGBA16)
  ImageRGBA16 img -> pixelMap dropTransparency img
  ImageYCbCr8 img -> promoteImage (convertImage img :: Image PixelRGB8)
  ImageCMYK8 _ -> error "ImageCMYK8"
  ImageCMYK16 _ -> error "ImageCMYK16"
  -- _ -> error "unsupported colorspace"

doubleImageAve :: (Pixel a, Integral (PixelBaseComponent a))
               => Image a -> Image a
doubleImageAve src = dst where
  w = imageWidth src
  h = imageHeight src
  n = 2
  dst = generateImage f (w * n) (h * n)
  f x y | if x == 0 then traceShow (x, y) False else False = undefined
  f x y = fromMaybe showError p where
    showError = error $ "doubleImageAve: internal error: " ++
                show ((x, y), (sx, sy), (sx11, sy11))
    s i = (itor i * 2 - itor (n - 1)) / itor (2 * n)
    (sx, sy) = (s x, s y)
    (sx11, sy11) = (floor sx, floor sy)
    rx, ry :: Rational
    rx = sx - itor sx11
    ry = sy - itor sy11
    p11 = pixelAtM sx11       sy11
    p12 = pixelAtM (sx11 + 1) sy11
    p21 = pixelAtM sx11       (sy11 + 1)
    p22 = pixelAtM (sx11 + 1) (sy11 + 1)
    valid x y = 0 <= x && x < w && 0 <= y && y < h
    pixelAtM x y = if valid x y then Just $ pixelAt src x y else Nothing
    p = bilinear <$> pure rx <*> pure ry <*> p11 <*> p12 <*> p21 <*> p22 <|>
        linear <$> pure rx <*> p11 <*> p12 <|>
        linear <$> pure rx <*> p21 <*> p22 <|>
        linear <$> pure ry <*> p11 <*> p21 <|>
        linear <$> pure ry <*> p21 <*> p22 <|>
        p11 <|> p12 <|> p21 <|> p22

bilinear :: (Pixel a, Integral (PixelBaseComponent a))
         => Rational -> Rational -> a -> a -> a -> a -> a
bilinear u v p q r s =
  mixWith (f v) (mixWith (f u) p q) (mixWith (f u) r s) where

linear :: (Pixel a, Integral (PixelBaseComponent a))
         => Rational -> a -> a -> a
linear u p q = mixWith (f u) p q

f t _ x y = floor (fromIntegral x * (1 - t) + fromIntegral y * t)

itor :: Int -> Rational
itor = fromInteger . toInteger

type PosX = Int
type PosY = Int
type PosDiff = Int
type Weight = Int

{-
resolvePixel :: Pixel a
             => Image a -> PosX -> PosY
             -> [(PosDiff, PosDiff, Weight)] -> a
resolvePixel i x y t ds = s / t where
  s = foldl' f  ds where
    f

mixPixels :: Pixel a => Image a -> Int -> Int -> Int -> [(Int, Int, Int)] -> a
mixPixels i x y t ds = s / t where
-}
