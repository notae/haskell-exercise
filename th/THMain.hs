{- THMain.hs -}

{-# LANGUAGE TemplateHaskell #-}

module THMain where

-- Import our template "pr"
import THPrintf (pr)

-- The splice operator $ takes the Haskell source code
-- generated at compile time by "pr" and splices it into
-- the argument of "putStrLn".
main = putStrLn ( $(pr "Hello") )
