{-
Waifu2xRepa.hs by @notae_c based on https://github.com/WL-Amigo/waifu2x-converter-cpp/blob/master/appendix/waifu2x-commented.py
MIT license, see https://github.com/nagadomi/waifu2x/blob/master/LICENSE

Use NN Model for RGB:
https://github.com/nagadomi/waifu2x/tree/master/models/anime_style_art_rgb
-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Functor.Identity
import Data.List             (foldl')
import Data.Word
import Debug.Trace           (trace)
import System.Environment    (getArgs)

import Control.Lens

import qualified Data.Array.Repa                  as R
import qualified Data.Array.Repa.Repr.Vector      as R
import qualified Data.Array.Repa.Specialised.Dim2 as R
import qualified Data.Array.Repa.Stencil          as R
import qualified Data.Array.Repa.Stencil.Dim2     as R

import Data.Array.Repa ((:.) (..), Any (..), Array, D, DIM2, DIM3, U, Z (..))
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

-- type ImgRGB  a = Source r a =>
--                  Array r DIM3 a
-- type ImgRGB8   = Array D DIM3 Word8
-- type ImgRGBF   = Array D DIM3 Float

-- type Img r a = Source r a =>
--                Array r DIM2 a
-- type Img8    = Array D DIM2 Word8
-- type ImgF    = Array D DIM2 Float

-- type PlaneS r = Img r Float
-- type PlaneU   = Img U Float
-- type Plane    = ImgF

-- type Filter r = PlaneS r -> Plane

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
  img8ToF :: (Source s Word8, Shape sh) => Array s sh Word8 -> Array D sh Float
  img8ToF = R.map ((/ 255) . fromRational . toRational)
  src2x :: Array D DIM3 Word8
  src2x = doubleImageNN src
  planes0 :: [Array U DIM2 Float]
  planes0 = map preProcessCh [0..k-1] where
    Z :. _ :. _ :. k = R.extent src
    preProcessCh :: Int -> Array U DIM2 Float
    preProcessCh = runIdentity . R.computeP . img8ToF . splitChs
    splitChs :: Int -> Array D DIM2 Word8
    splitChs c = R.slice src2x (Any :. (c::Int))

  -- main process
  planesN :: [Array U DIM2 Float]
  planesN = foldl' procStep planes0 (zip model [0..]) where
    procStep :: (Source s Float) => [Array s DIM2 Float] -> (Step, Int) -> [Array U DIM2 Float]
    procStep inPlanes (step, i) |
      inPlanes `seqList` traceStep inPlanes (step, i) = undefined
    procStep inPlanes (step, _) = inPlanes `seqList`
      zipWith3 procOutPlane (step ^. weight) (step ^. bias) [0..] where
        procOutPlane :: [Kernel] -> Float -> Int -> Array U DIM2 Float
        -- procOutPlane _ _ j | traceOutPlane step j = undefined
        procOutPlane ws b _ = runIdentity $ R.computeP $
                              cutNeg . addBias b . sumP $
                              zipWith convolve ws inPlanes

  -- post-process
  dest :: Array F DIM3 Word8
  dest = runIdentity $ R.computeP $ mergeChs $ map (f . clip) planesN where
    clip :: (Source s Float) => Array s DIM2 Float -> Array D DIM2 Word8
    clip = R.map (floor . (* 255) . clamp 0 1)
    f :: (Source s Word8) => Array s DIM2 Word8 -> Array D DIM3 Word8
    f ch = R.reshape (R.extent ch :. (1::Int)) ch
    mergeChs :: (Source s Word8) => [Array s DIM3 Word8] -> Array D DIM3 Word8
    mergeChs [r, g, b] = r R.++ g R.++ b
    mergeChs _ = error "waifu2xMain.g: ERROR: number of elements must be 3"

  -- trace output
  traceStep inPlanes (step, i) =
    trace ("procStep: " ++ show i ++ "," ++
           show (length inPlanes) ++ "," ++
           show (step ^. nInputPlane) ++ "," ++
           show (step ^. nOutputPlane) ++ "," ++
           show (length (step ^. weight)))
          False
  traceOutPlane step j =
    trace ("procOutPlane: " ++ show j ++ "/" ++ show (step ^. nOutputPlane)) False

seqList :: [a] -> b -> b
seqList []     b = b
seqList (a:as) b = a `seq` seqList as b

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
  Z :. h :. w :. k = R.extent src
  sh' = Z :. h*2 :. w*2 :. k
  f (Z :. y :. x :. c) = src R.! (Z :. (y `div` 2) :. (x `div` 2) :. c)

-- padEdge3 :: (Source s a) => Int -> Array s DIM3 a -> Array D DIM3 a
-- padEdge3 n src = R.fromFunction sh' f where
--   Z :. h :. w :. k = R.extent src
--   sh' = Z :. h+n*2 :. w+n*2 :. k
--   f (Z :. y :. x :. c) = src R.! (Z :. y' :. x' :. c) where
--     !x' = clamp n (n + w - 1) x - n
--     !y' = clamp n (n + h - 1) y - n

padEdge :: (Source s a) => Int -> Array s DIM2 a -> Array D DIM2 a
padEdge n src = R.fromFunction sh' f where
  Z :. h :. w = R.extent src
  sh' = Z :. h+n*2 :. w+n*2
  f (Z :. y :. x) = src R.! (Z :. y' :. x') where
    !x' = clamp n (n + w - 1) x - n
    !y' = clamp n (n + h - 1) y - n

convolve :: (Source s Float) => Kernel -> Array s DIM2 Float -> Array R.PC5 DIM2 Float
convolve k = R.mapStencil2 R.BoundClamp st where
  !st = makeStencil2FromKernel' 3 3 k -- TBD: take kW, kH

{-# INLINE makeStencil2FromKernel #-}
makeStencil2FromKernel :: Int -> Int -> Kernel -> R.Stencil DIM2 Float
makeStencil2FromKernel w h mat = R.makeStencil2 h w f where
  !sh = Z :. h :. w
  !ary = R.fromListUnboxed sh (concat mat)
  !f = stfunc sh ary (h `div` 2) (w `div` 2)

{-# INLINE makeStencil2FromKernel' #-}
makeStencil2FromKernel' :: Int -> Int -> Kernel -> R.Stencil DIM2 Float
makeStencil2FromKernel' !w !h !mat = R.makeStencil2' h w f where
  !sh = Z :. h :. w
  !ary = R.fromListUnboxed sh (concat mat)
  !f = stfunc' sh ary (h `div` 2) (w `div` 2)

{-# INLINE stfunc #-}
stfunc :: Source r a => DIM2 -> Array r DIM2 a -> Int -> Int -> DIM2 -> Maybe a
stfunc sh ary cy cx (_ :. y :. x) =
  if R.isInside2 sh i then Just (ary R.! i) else Nothing where
    !i = Z :. y + cy :. x + cx

{-# INLINE stfunc' #-}
stfunc' :: Source r a => DIM2 -> Array r DIM2 a -> Int -> Int -> DIM2 -> a
stfunc' !sh !ary !cy !cx (_ :. y :. x) = ary R.! (Z :. y + cy :. x + cx)

a1 :: Array U DIM2 Float
a1 = R.fromListUnboxed (Z:.8:.8) [1..64]

s1 :: R.Stencil DIM2 Float
s1 = makeStencil2FromKernel 3 3 [[0,0,0],[0,1,0],[0,0,0]]
s2 :: R.Stencil DIM2 Float
s2 = makeStencil2FromKernel 3 3 [[0.125,0.125,0.125],[0.125,0,0.125],[0.125,0.125,0.125]]

addBias :: (Source s Float) => Float -> Array s DIM2 Float -> Array D DIM2 Float
addBias b = R.map (+ b)

cutNeg :: (Source s Float) => Array s DIM2 Float -> Array D DIM2 Float
cutNeg = R.map $ \y -> max y 0 + 0.1 * min y 0

sumP :: (Source s Float) => [Array s DIM2 Float] -> Array U DIM2 Float
sumP ps = runIdentity $ R.sumP $ R.fromFunction (sh :. l) f where
  !l = length ps
  !sh = R.extent (head ps)
  !ary = R.fromListVector (Z :. l) ps
  f (_ :. y :. x :. i) = (ary R.! (Z :. i)) R.! (Z :. y :. x)

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
