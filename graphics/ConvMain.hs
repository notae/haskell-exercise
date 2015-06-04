-- module ConvMain where

import System.Environment  (getArgs)

import Graphics

main :: IO ()
main = do
  [cmd, path] <- getArgs
  let (Just conv) = lookup cmd converters
  convImageFile conv path (cmd ++ "_" ++ path)
