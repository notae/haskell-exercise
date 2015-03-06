{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module PatternSynonyms where

-- defined as pattern
pattern P1 = 1
pattern P2 = 2
-- just covered
f :: Int -> String
f P1 = "one"
f P2 = "two"
f _  = "unknown"

-- defined as variables for comarison
p1 = 1
p2 = 2
-- causes "Warning: Pattern match(es) are overlapped"
g :: Int -> String
g p1 = "one"
g p2 = "two"
g _  = "unknown"

-- defined as pattern with tupple
pattern F x y = (x, y)
-- causes "Warning: Pattern match(es) are non-exhaustive"
h :: (Int, Int) -> Int
h (F 2 3) = 999
h (F x y) = x + y

-- defined without pattern synonyms for comparison
-- causes no warnings
h' :: (Int, Int) -> Int
h' (2, 3) = 999
h' (x, y) = x + y
-- an similar example
fn :: Maybe Int -> Int
fn (Just n) = n
fn Nothing  = -1

-- unidirectional pattern
pattern Head x <- x:_
head' :: [a] -> Maybe a
head' (Head x) = Just x
head' []        = Nothing
-- to avoid the warning
-- head' _        = Nothing

initLast :: [t] -> Maybe ([t], t)
initLast [] = Nothing
initLast xs = Just (init xs, last xs)
pattern xs ::: x <- (initLast -> Just (xs,x))
il :: [a] -> Maybe a
il [] = Nothing
il (_ ::: x) = Just x
-- to avoid the warning
-- il _ = Nothing
