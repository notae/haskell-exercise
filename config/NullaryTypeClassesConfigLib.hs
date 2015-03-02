{-# LANGUAGE NullaryTypeClasses #-}

module NullaryTypeClassesConfigLib where

class Config where
  putLog :: String -> IO ()
  putLog _ = return ()

proc :: Config => Int -> Int -> IO ()
proc x y = do
  print x
  print y
  putLog $ "x+y=" ++ show (x + y)
