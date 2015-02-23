module FileCopy where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFileName, outFileName] -> copyFile inFileName outFileName
    _                         -> error "invalid arguments"

copyFile :: FilePath -> FilePath -> IO ()
copyFile inFileName outFileName = do
  content <- readFile inFileName
  writeFile outFileName content
