module RTLILParser.Primitives(
    pWs
   ,pNonWs
   ,pMaybeWs
   ,pEol
   ,pOctal
   ,pEscapedChar
   ,advanceToNextToken
   ,advanceToFirstToken
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

pMaybeWs :: Parser String
pMaybeWs = many (oneOf " \t") <?> "MaybeWs"

pWs :: Parser String
pWs = many1 (oneOf " \t") <?> "Ws"

-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/frontends/rtlil/rtlil_lexer.l#L88C1-L88C17
pNonWs :: Parser Char
pNonWs = noneOf " \t\r\n" <?> "NonWs"

pEol :: Parser ()
pEol = void (oneOf "\r\n") <?> "Eol"

-- a comment begins with # and ends at the end of the line
-- a comment can be be inline, but must still end at the end of the line
pComment :: Parser String
pComment = p <?> name where
    name = "Comment"
    p =
        do
        char '#'
        comment <- many (noneOf "\r\n")
        pEol
        return comment

advanceToNextToken :: Parser ()
advanceToNextToken = p <?> name where
    name = "AdvanceToNextToken"
    p =
        void (pMaybeWs *> many1 (void pComment <|> (pEol <* pMaybeWs)))

advanceToFirstToken :: Parser ()
advanceToFirstToken = p <?> name where
    name = "AdvanceToFirstToken"
    p =
        void (pMaybeWs *> many (void pComment <|> (pEol <* pMaybeWs)))