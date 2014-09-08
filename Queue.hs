-- Example of 2-List Queue

module Queue where

-- Queue by 2-List
--   Amortized cost  : O(1)
--   Worst-case cost : O(n)

data Queue a = Q [a] [a] deriving (Show)

empty :: Queue a
empty = Q [] []

{-|
>>> qnull empty
True
>>> qnull $ enq 1 empty
False
-}
qnull :: Queue a -> Bool
qnull (Q is os) = null is && null os

{-|
>>> enq 2 $ enq 1 empty
Q [2,1] []
-}
enq :: a -> Queue a -> Queue a
enq a (Q is os) = Q (a:is) os

{-|
>>> deq $ enq 2 $ enq 1 empty
(1,Q [] [2])
-}
deq :: Queue a -> (a, Queue a)
deq (Q is [])     = deq $ Q [] $ reverse is
deq (Q is (a:os)) = (a, Q is os)
