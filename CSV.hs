module CSV where

import Text.Parsec
import Text.Parsec.Text.Lazy ()
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

type CSV = [[String]]

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'


{-|
>>> parseCSV (T.pack "a,b,c\nd,e,f\n")
Right [["a","b","c"],["d","e","f"]]
-}
parseCSV :: Text -> Either ParseError CSV
parseCSV input = parse csvFile "(unknown)" input
