{-# LANGUAGE ImplicitParams #-}

module ImplicitParams where

type LogFunction = String -> IO ()

proc :: (?putLog :: LogFunction) => Int -> Int -> IO ()
proc x y = do
  print x
  print y
  ?putLog $ "x+y=" ++ show (x + y)

useProc :: IO ()
useProc = proc 2 3 where
  ?putLog = \s -> putStr "[Log] " >> putStrLn s
  -- Function style definitions are not allowed
  -- ?putLog s = putStr "[Log] " >> putStrLn s
