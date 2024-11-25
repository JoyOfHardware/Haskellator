module Util(
    binaryStringToInt,
    pEscapedChar,
    pEscapedChar
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

pOctal :: Parser Char
pOctal = do
    digits <- count 1 digit -- At least 1 digit
    moreDigits <- option "" (count 2 digit) -- Up to 3 digits total
    case octalStringToInt (digits ++ moreDigits) of
        Just value -> return $ toEnum value -- Convert integer to a Char
        Nothing    -> fail "Invalid octal escape sequence"

pEscapedChar :: Parser Char
pEscapedChar = do
    char '\\' -- Match the backslash
    choice
        [ char 'n' >> return '\n'   -- \n → newline
        , char 't' >> return '\t'   -- \t → tab
        , try pOctal                -- \123 → octal escape
        , anyChar                   -- Any other escaped char (e.g., \\)
        ]
