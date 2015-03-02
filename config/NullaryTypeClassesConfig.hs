{-# LANGUAGE NullaryTypeClasses #-}

module NullaryTypeClassesConfig where

import NullaryTypeClassesConfigLib

instance Config where
  putLog s = putStr "[LOG] " >> putStrLn s

{-|
>>> useProc
2
3
[LOG] x+y=5
-}
useProc :: IO ()
useProc = proc 2 3
