-- Example for GenericPretty

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.PrettyPrint.GenericPretty as GP
import qualified Text.PrettyPrint as TP

{-|
Print user defined data with supported filed type

>>> GP.pp (UserData [1..100])
UserData {unUserData = [1,2,3,4,5,6,7,8,9,10,11,12,
                        13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
                        30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,
                        47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
                        64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                        81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,
                        98,99,100]}
-}

data UserData = UserData { unUserData :: [Int] }
              deriving (Show, GP.Generic)

instance GP.Out UserData

{-|
Print user defined data with non-suppoted field type by ad hoc code


>>> GP.pp (UserData2 (Set.fromList [1..100]))
UserData2 {unUserData2 = fromList [1,2,3,4,5,6,7,8,9,
                                   10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
                                   25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
                                   40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
                                   55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
                                   70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,
                                   85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,
                                   100]}
-}

data UserData2 = UserData2 { unUserData2 :: Set Int }
               deriving (Show, GP.Generic)

instance GP.Out a => GP.Out (Set a) where
  docPrec n s = TP.text "fromList" TP.<+> GP.docPrec n (Set.toList s)
  doc s = GP.docPrec 0 s

instance GP.Out UserData2
