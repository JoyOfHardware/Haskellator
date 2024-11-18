-- this parser largely references:
-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/docs/source/yosys_internals/formats/rtlil_text.rst
module Haskellator(a) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/frontends/rtlil/rtlil_lexer.l#L88C1-L88C17
nonws :: Parser Char
nonws = noneOf " \t\r\n"

pPublicId :: Parser String
pPublicId = char '\\' *> many1 nonws

pAutogenId :: Parser String
pAutogenId = char '$' *> many1 nonws

decimalDigit :: Parser Char
decimalDigit = oneOf "0123456789"

binaryDigit :: Parser Char
binaryDigit = oneOf "01xzm-"

-- an integer can be positive or negative
pInteger :: Parser Int
pInteger = read <$> (pNegInt <|> pPosInt)
    where
        pIntAtom = many1 decimalDigit
        pNegInt = char '-' *> pIntAtom
        pPosInt = pIntAtom

a :: Int
a = 3