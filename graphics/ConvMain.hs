-- module ConvMain where

import System.Environment  (getArgs)

import Graphics

main :: IO ()
main = do
  [cmd, path] <- getArgs
  case cmd of
   "simple" -> doubleImageFileSimple path
   "bilinear" -> doubleImageFileBilinear path
   _ -> error "unknown command"
