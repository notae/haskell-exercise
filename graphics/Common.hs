module Common where

import Codec.Picture
import Codec.Picture.Metadata (Metadatas)
import Codec.Picture.Types

--
-- Image Processing
--

{-|
>>> fmap (clamp 2 5) [0, 3, 7]
[2,3,5]
-}
clamp :: Ord a => a -> a -> a -> a
clamp l h = min h . max l

getImageSize :: Image a -> (Int, Int)
getImageSize dimg = (imageWidth  dimg, imageHeight dimg)

--
-- Logging
--

dumpTitle :: String -> IO ()
dumpTitle name = putStrLn $ "== " ++ name ++ " =="

dump :: Show a => String -> a -> IO ()
dump name val = putStrLn $ name ++ ": " ++ show val

dumpImageInfo :: String -> DynamicImage -> Metadatas -> IO ()
dumpImageInfo title dimg md = do
  dumpTitle title
  let (w, h) = dynamicMap getImageSize dimg
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
