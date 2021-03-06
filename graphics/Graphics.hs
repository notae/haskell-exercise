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

type ImageConverter = DynamicImage -> DynamicImage

converters :: [(String, ImageConverter)]
converters = [ ("simple2x",   convSimple2x)
             , ("bilinear2x", convBilinearNxRGB8 2)
             , ("bilinear3x", convBilinearNxRGB8 3)
             ]

convImageFile :: ImageConverter -> FilePath -> FilePath -> IO ()
convImageFile conv path path' = do
  Right dimg <- readImage path
  let dimg' = conv dimg
  savePngImage path' dimg'

convSimple2x :: ImageConverter
convSimple2x = dynamicPixelMap doubleImageSimple

doubleImageSimple :: Pixel a => Image a -> Image a
doubleImageSimple src = dst where
  w = imageWidth src
  h = imageHeight src
  dst = generateImage f (w * 2) (h * 2)
  f x y = pixelAt src (x `div` 2) (y `div` 2)

convBilinearNxRGB8 :: Int -> ImageConverter
convBilinearNxRGB8 n = ImageRGB8 . convBilinearNx n . convToImageRGB8

convToImageRGB8 :: DynamicImage -> Image PixelRGB8
convToImageRGB8 dimg = case dimg of
  ImageY8 _ -> error "ImageY8"
  ImageY16 _ -> error "ImageY16"
  ImageYF _ -> error "ImageYF"
  ImageYA8 _ -> error "ImageYA8"
  ImageYA16 _ -> error "ImageYA16"
  ImageRGB8 img -> img
  ImageRGB16 _ -> error "ImageRGB16"
  ImageRGBF _ -> error "ImageRGBF"
  ImageRGBA8 img -> pixelMap dropTransparency img
  ImageRGBA16 _ -> error "ImageRGBA16"
  ImageYCbCr8 img -> convertImage img
  ImageCMYK8 _ -> error "ImageCMYK8"
  ImageCMYK16 _ -> error "ImageCMYK16"

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

convBilinearNx :: (Pixel a, Integral (PixelBaseComponent a))
                => Int -> Image a -> Image a
convBilinearNx n src = dst where
  w = imageWidth src
  h = imageHeight src
  dst = generateImage f (w * n) (h * n)
  f x y | (x == 0 && y `mod` 64 == 0) && traceShow (x, y) False = undefined
  f x y = fromMaybe showError p where
    showError = error $ "convBilinearNx: internal error: " ++
                show ((x, y), (sx, sy), (sx11, sy11))
    s i = (itor i * 2 - itor (n - 1)) / itor (2 * n)
    (sx, sy) = (s x, s y)
    (sx11, sy11) = (floor sx, floor sy)
    rx = sx - itor sx11
    ry = sy - itor sy11
    p11 = pixelAtM sx11       sy11
    p12 = pixelAtM (sx11 + 1) sy11
    p21 = pixelAtM sx11       (sy11 + 1)
    p22 = pixelAtM (sx11 + 1) (sy11 + 1)
    valid px py = 0 <= px && px < w && 0 <= py && py < h
    pixelAtM px py = if valid px py then Just $ pixelAt src px py else Nothing
    p = bilinear2 <$> pure rx <*> pure ry <*> p11 <*> p12 <*> p21 <*> p22 <|>
        bilinear <$> pure rx <*> p11 <*> p12 <|>
        bilinear <$> pure rx <*> p21 <*> p22 <|>
        bilinear <$> pure ry <*> p11 <*> p21 <|>
        bilinear <$> pure ry <*> p21 <*> p22 <|>
        p11 <|> p12 <|> p21 <|> p22
    -- itor :: Int -> Rational
    itor :: Int -> Float
    itor = fromInteger . toInteger
    {-# INLINE itor #-}

bilinear2 :: (RealFrac t, Pixel a, Integral (PixelBaseComponent a))
         => t -> t -> a -> a -> a -> a -> a
bilinear2 u v p q r s =
  mixWith (mix v) (mixWith (mix u) p q) (mixWith (mix u) r s) where
-- {-# INLINE bilinear2 #-}
{-# SPECIALIZE bilinear2 :: Float -> Float -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 #-}
{-# SPECIALIZE bilinear2 :: Rational -> Rational -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 #-}

bilinear :: (RealFrac t, Pixel a, Integral (PixelBaseComponent a))
         => t -> a -> a -> a
bilinear u p q = mixWith (mix u) p q
-- {-# INLINE bilinear #-}
{-# SPECIALIZE bilinear :: Float -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 #-}
{-# SPECIALIZE bilinear :: Rational -> PixelRGB8 -> PixelRGB8 -> PixelRGB8 #-}

mix :: (RealFrac t, Integral a) => t -> Int -> a -> a -> a
mix t _ x y = floor (fromIntegral x * (1 - t) + fromIntegral y * t)
{-# INLINE mix #-}
-- {-# SPECIALIZE mix :: Float -> Int -> PixelBaseComponent PixelRGB8 -> PixelBaseComponent PixelRGB8 -> PixelBaseComponent PixelRGB8 #-}
-- {-# SPECIALIZE mix :: Rational -> Int -> PixelBaseComponent PixelRGB8 -> PixelBaseComponent PixelRGB8 -> PixelBaseComponent PixelRGB8 #-}
