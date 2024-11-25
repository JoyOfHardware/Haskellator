module Util(
    binaryStringToInt,
    octalStringToInt,
    ) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Data.List (foldl')

binaryStringToInt :: String -> Int
binaryStringToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

octalStringToInt :: String -> Maybe Int
octalStringToInt str
    | all (`elem` ['0'..'7']) str = Just $ foldl' (\acc x -> acc * 8 + digitToInt x) 0 str
    | otherwise                   = Nothing
