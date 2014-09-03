-- Example of class Read

module Read where

import Data.Set (Set)
import Data.Map (Map)

{-|
>>> test1
[1,2,3]
-}
test1 :: [Int]
test1 = read "[1, 2, 3]"

{-|
>>> test2
*** Exception: Prelude.read: no parse
-}
test2 :: [Int]
test2 = read "[1..3]"

{-|
>>> test3
[[1,2,3],[4,5,6],[7,8,9]]
-}
test3 :: [[Int]]
test3 = read "[[1,2,3],[4,5,6],[7,8,9]]"

{-|
>>> test4
fromList [1,2,3]
-}
test4 :: Set Int
test4 = read "fromList [1,2,3]"

{-|
>>> test5
fromList [(1,1),(2,4),(3,9)]
-}
test5 :: Map Int Int
test5 = read "fromList [(1,1),(2,4),(3,9)]"

{-|
>>> test5'
fromList [(1,1),(2,4),(3,9)]
-}
test5' :: Set (Int, Int)
test5' = read "fromList [(1,1),(2,4),(3,9)]"

data Foo = Foo Int Bool deriving (Show, Read)

{-|
>>> test6
Foo 123 True
-}
test6 :: Foo
test6 = read "Foo 123 True"

data Bar = Bar { barInt :: Int, barBool :: Bool } deriving (Show, Read)

{-|
>>> test7
Bar {barInt = 123, barBool = True}
-}
test7 :: Bar
test7 = read "Bar {barInt = 123, barBool = True}"
