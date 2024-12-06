-- this parser largely references:
-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/docs/source/yosys_internals/formats/rtlil_text.rst
module RTLILParser.Parser(a, val) where


import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import RTLILParser.AST(
    AutoIdxStmt(..),    ParamStmt(..), AutogenId(..),
    Constant(..),       CellStmt(..),  PublicId(..),
    AttrStmt(..),       Value(..),     Id(..),
    CellId(..),         CellType(..), WireId(..),
    SigSpec(..)
    )
import Util(binaryStringToInt)
import RTLILParser.Primitives(pEscapedChar)

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

pId :: Parser Id
pId = Public  <$> pPublicId
  <|> Autogen <$> pAutogenId

pWireId :: Parser WireId
pWireId = WireId <$> pId

decimalDigit :: Parser Char
decimalDigit = oneOf "0123456789"

-- update in the future to support 4 state logic
-- by converting x and z to 0 and warning about it.
pBinaryDigit :: Parser Char
pBinaryDigit = oneOf "01"

pString :: Parser String
pString =
    between delimiter delimiter parseString
    where
        delimiter = char '"'
        parseString = many (pEscapedChar <|> noneOf "\\\"")


pValue :: Parser Value
pValue = Value  <$> pInteger
                <*> (binaryStringToInt <$> many1 pBinaryDigit)

pInteger :: Parser Int
pInteger = do
    sign <- optionMaybe (char '-')
    digits <- many1 digit
    let value = read digits
    return $ case sign of
        Just _  -> -value
        Nothing ->  value

pConstant :: Parser Constant
pConstant =
    try (ConstantValue <$> pValue)
    <|> (ConstantInteger <$> pInteger)
    <|> (ConstantString <$> pString)

pAutoIdxStmt :: Parser AutoIdxStmt
pAutoIdxStmt = AutoIdxStmt <$> (string "autoidx" *> pWs *> pInteger <* pEol)

pModuleStmt :: Parser Id
pModuleStmt = string "module" *> pWs *> pId <* pEol

pModuleEndStmt :: Parser ()
pModuleEndStmt = void (string "end")

pParamStmt :: Parser ParamStmt
pParamStmt = ParamStmt
    <$> (string "parameter" *> pWs *> pId)
    <*> optionMaybe (pWs *> pConstant)
    <*  pEol

pAttrStmt :: Parser AttrStmt
pAttrStmt = AttrStmt
    <$> (string "attribute" *> pWs *> pId)
    <*> (pWs *> pConstant)
    <*  pEol

pCellStmt :: Parser CellStmt
pCellStmt = do
    _ <- string "cell"
    _ <- pWs
    cellId <- CellId <$> pId
    _ <- pWs
    cellType <- CellType <$> pId
    _ <- pEol
    return $ CellStmt cellId cellType


-- pModuleStmt :: Parser ()
-- pModuleStmt = 

-- val = parse pInteger "pInteger" "721"
val = parse pModuleStmt "pModuleStmt" "module \\top\n"

a :: Int
a = 3