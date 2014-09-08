-- Example of 2-List Queue

module Queue where

-- Queue by 2-List
--   Amortized cost  : O(1)
--   Worst-case cost : O(n)

data Queue a = Q [a] [a] deriving (Show)

empty :: Queue a
empty = Q [] []

{-|
>>> enq 2 $ enq 1 empty
Q [2,1] []
-}
enq :: a -> Queue a -> Queue a
enq a (Q ins outs) = Q (a:ins) outs

{-|
>>> deq $ enq 2 $ enq 1 empty
(1,Q [] [2])
-}
deq :: Queue a -> (a, Queue a)
deq (Q ins [])       = deq $ Q [] $ reverse ins
deq (Q ins (a:outs)) = (a, Q ins outs)
