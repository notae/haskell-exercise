{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module ViewPatterns where

data D = D { dFirst :: Int, dSecond :: Bool } deriving (Show, Eq)

view :: D -> (Int, Bool)
view D{..} = (dFirst, dSecond)

-- GHC says "Warning: Pattern match(es) are non-exhaustive"
f :: D -> Int
f (view -> (i, True)) = i * 2
f (view -> (i, False)) = i
-- f (view -> (i, _)) = i
-- f (view -> _) = undefined

-- no warning
g :: (Int, Bool) -> Int
g (i, True) = i * 2
g (i, False) = i
