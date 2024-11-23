-- this parser largely references:
-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/docs/source/yosys_internals/formats/rtlil_text.rst
module Haskellator(a, val) where

import Data.Char (digitToInt)
import Data.List (foldl')

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import RtlilAstTypes(
    PublicId(..), 
    AutogenId(..),
    AutoIdxStmt(..),
    Value(..)
    )

binaryStringToInt :: String -> Int
binaryStringToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/frontends/rtlil/rtlil_lexer.l#L88C1-L88C17
nonws :: Parser Char
nonws = noneOf " \t\r\n"

pWs :: Parser String
pWs = many1 (oneOf " \t")

pEol :: Parser String
pEol = many1 (oneOf "\r\n")

pPublicId   :: Parser PublicId
pPublicId   = PublicId <$> (char '\\' *> many1 nonws)

pAutogenId  :: Parser AutogenId
pAutogenId  = AutogenId <$> (char '$' *> many1 nonws)

decimalDigit :: Parser Char
decimalDigit = oneOf "0123456789"

-- update in the future to support 4 state logic
-- by converting x and z to 0 and warning about it.
binaryDigit :: Parser Char
binaryDigit = oneOf "01"

pValue :: Parser Value
pValue = Value  <$> pInteger 
                <*> (binaryStringToInt <$> many1 binaryDigit)

pInteger :: Parser Int
pInteger = do
    sign <- optionMaybe (char '-')
    digits <- many1 digit
    let value = read digits
    return $ case sign of
        Just _  -> -value
        Nothing ->  value

pAutogenIdx :: Parser AutoIdxStmt
pAutogenIdx = AutoIdxStmt <$> (string "autoidx" *> pWs *> pInteger <* pEol)

pModuleEndStmt :: Parser ()
pModuleEndStmt = void (string "end")

-- pModuleEndStmt :: Parser String
-- pModuleEndStmt = string "end" <* pEol

val = parse pInteger "pInteger" "721"

a :: Int
a = 3