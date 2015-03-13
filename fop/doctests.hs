module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "Fop12_1.hs"
               , "Fop12_2.hs"
               , "Fop12_3.hs"
               , "Fop12_4.hs"
               ]
