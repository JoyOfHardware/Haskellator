-- this parser largely references:
-- https://yosyshq.readthedocs.io/projects/yosys/en/stable/appendix/rtlil_text.html
module RTLILParser.Parser(a, val) where


import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import RTLILParser.AST(
    AutoIdxStmt(..),    ParamStmt(..),  AutogenId(..),
    Constant(..),       CellStmt(..),   PublicId(..),
    AttrStmt(..),       Value(..),      Id(..),
    CellId(..),         CellType(..),   WireId(..),
    SigSpec(..),        Slice(..)
    )
import Util(binaryStringToInt)
import RTLILParser.Primitives(pEscapedChar)

-- https://github.com/YosysHQ/yosys/blob/111b747d2797238eadf541879848492a9d34909a/frontends/rtlil/rtlil_lexer.l#L88C1-L88C17
nonws :: Parser Char
nonws = noneOf " \t\r\n"

pMaybeWs :: Parser String
pMaybeWs = many (oneOf " \t")

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

-- Parse a single slice
pSlice :: Parser Slice
pSlice =
    Slice
    <$> (char '[' *> pMaybeWs *> pInteger <* pMaybeWs)
    <*> (optionMaybe (char ':' *> pInteger) <* pMaybeWs <* char ']')

pSigSpecConcat :: Parser SigSpec
pSigSpecConcat = do
    _           <- char '{' <* pWs
    sigspecs    <- pSigSpec `sepBy` pWs
    _           <- pWs <* char '}'
    return $ SigSpecConcat sigspecs

applySlices :: SigSpec -> Parser SigSpec
applySlices base = do
    maybeSlice <- optionMaybe pSlice
    case maybeSlice of
        Nothing -> return base
        Just slice -> applySlices (SigSpecSlice base slice)

pSingleSigSpec :: Parser SigSpec
pSingleSigSpec = do
    baseSigSpec <- (SigSpecConstant <$> pConstant) 
                   <|>
                   (SigSpecWireId   <$> pWireId)
    applySlices baseSigSpec

pSigSpec :: Parser SigSpec
pSigSpec = 
    try pSigSpecConcat -- Check for concatenation first
    <|> pSingleSigSpec -- Otherwise parse a single sigspec


-- would correspond to `123456789[0:9][0:8]`
exampleSigSpecSlice = 
    SigSpecSlice 
        (
            SigSpecSlice 
            (SigSpecConstant (ConstantInteger 123456789)) 
                (Slice 0 $ Just 9)
        )
            (Slice 0 $ Just 8)

-- val = parse pInteger "pInteger" "721"
-- val = parse pModuleStmt "pModuleStmt" "module \\top\n"
val = parse pSigSpec "pSigSpecSlice" "123456789[0:9][0:8]"

a :: Int
a = 3