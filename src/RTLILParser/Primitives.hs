module RTLILParser.Primitives(
    pOctal,
    pEscapedChar
) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)
import Util(binaryStringToInt, octalStringToInt)


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
        [ char 'n' >> return '\n'
        , char 't' >> return '\t'
        , try pOctal             
        , anyChar                
        ]