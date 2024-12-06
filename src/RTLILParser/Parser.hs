module RTLILParser.Parser(a, val) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import RTLILParser.AST(
    AutoIdxStmt(..)    ,ParamStmt(..)  ,AutogenId(..)
   ,Constant(..)       ,CellStmt(..)   ,PublicId(..)
   ,AttrStmt(..)       ,Value(..)      ,Id(..)
   ,CellId(..)         ,CellType(..)   ,WireId(..)
   ,SigSpec(..)        ,Slice(..)      ,ConnStmt(..)
   ,WireOption(..)     ,WireStmt(..)   ,Wire(..)
   ,MemoryOption(..)   ,MemoryStmt(..) ,Memory(..)
   ,MemoryID(..)
    )
import Util(binaryStringToInt)
import RTLILParser.Primitives(
    pWs
   ,pNonWs
   ,pMaybeWs
   ,pEol
   ,pOctal
   ,pEscapedChar
    )

-- taken from: https://yosyshq.readthedocs.io/projects/yosys/en/0.47/appendix/rtlil_text.html
-- parsers below are split int sections from the above link

-- identifiers
pId :: Parser Id
pId = Public  <$> pPublicId
  <|> Autogen <$> pAutogenId

pPublicId   :: Parser PublicId
pPublicId   = PublicId <$> (char '\\' *> many1 pNonWs)

pAutogenId  :: Parser AutogenId
pAutogenId  = AutogenId <$> (char '$' *> many1 pNonWs)

-- values
pValue :: Parser Value
pValue = do
    width <- many1 pDecimalDigit
    _ <- char '\''
    value <- many pBinaryDigit
    return $ Value (read width) (binaryStringToInt value)

pDecimalDigit :: Parser Char
pDecimalDigit = oneOf "0123456789"

-- update in the future to support 4 state logic
-- by converting x and z to 0 and warning about it.
pBinaryDigit :: Parser Char
pBinaryDigit = oneOf "01"

pInteger :: Parser Int
pInteger = do
    sign <- optionMaybe (char '-')
    digits <- many1 pDecimalDigit
    let value = read digits
    return $ case sign of
        Just _  -> -value
        Nothing ->  value

-- strings
pString :: Parser String
pString =
    between delimiter delimiter parseString
    where
        delimiter = char '"'
        parseString = many (pEscapedChar <|> noneOf "\\\"")

-- comments
-- file

-- Autoindex statements
pAutoIdxStmt :: Parser AutoIdxStmt
pAutoIdxStmt = AutoIdxStmt <$> (string "autoidx" *> pWs *> pInteger <* pEol)

-- Module
pModuleStmt :: Parser Id
pModuleStmt = string "module" *> pWs *> pId <* pEol

pParamStmt :: Parser ParamStmt
pParamStmt = ParamStmt
    <$> (string "parameter" *> pWs *> pId)
    <*> optionMaybe (pWs *> pConstant)
    <*  pEol

pConstant :: Parser Constant
pConstant =
    try (ConstantValue <$> pValue)
    <|> (ConstantInteger <$> pInteger)
    <|> (ConstantString <$> pString)

pModuleEndStmt :: Parser ()
pModuleEndStmt = void (string "end")

-- Attribute statements
pAttrStmt :: Parser AttrStmt
pAttrStmt = AttrStmt
    <$> (string "attribute" *> pWs *> pId)
    <*> (pWs *> pConstant)
    <*  pEol

-- Signal Specifications
pSigSpec :: Parser SigSpec
pSigSpec = do
    baseSigSpec <- (SigSpecConstant <$> pConstant)
                   <|>
                   (SigSpecWireId   <$> pWireId)
                   <|>
                   pSigSpecConcat
    applySlices baseSigSpec

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

pSlice :: Parser Slice
pSlice =
    Slice
    <$> (char '[' *> pMaybeWs *> pInteger <* pMaybeWs)
    <*> (optionMaybe (char ':' *> pInteger) <* pMaybeWs <* char ']')

-- Connections
pConnStmt :: Parser ConnStmt
pConnStmt = ConnStmt
    <$> (string "connect" *> pWs *> pSigSpec)
    <*> (pWs *> pSigSpec)
    <*  pEol

-- Wires
pWire :: Parser Wire
pWire = do
    attrs <- many pAttrStmt
    wireStmt <- pWireStmt
    return $ Wire wireStmt attrs

pWireStmt :: Parser WireStmt
pWireStmt =
    WireStmt
    <$  string "wire"
    <*  pWs
    <*> (WireId <$> pId)
    <*  pWs
    <*> many pWireOption
    <*  pEol

pWireId :: Parser WireId
pWireId = WireId <$> pId

pWireOption :: Parser WireOption
pWireOption =
    try (WireOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|>
    try (WireOptionOffset <$> (string "offset" *> pWs *> pInteger)) <|>
    try (WireOptionInput  <$> (string "input"  *> pWs *> pInteger)) <|>
    try (WireOptionOutput <$> (string "output" *> pWs *> pInteger)) <|>
    try (WireOptionInout  <$> (string "inout"  *> pWs *> pInteger)) <|>
    (string "upto"      *> return WireOptionUpto)                   <|>
    (string "signed"    *> return WireOptionSigned)

-- Memories
pMemory :: Parser Memory
pMemory = do
    attrs <- many pAttrStmt
    memoryStmt <- pMemoryStmt
    return $ Memory memoryStmt attrs

pMemoryStmt :: Parser MemoryStmt
pMemoryStmt =
    MemoryStmt
    <$  string "memory"
    <*  pWs
    <*> (MemoryID <$> pId)
    <*  pWs
    <*> many pMemoryOption
    <*  pEol

pMemoryOption :: Parser MemoryOption
pMemoryOption =
    try (MemoryOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|>
    try (MemoryOptionSize   <$> (string "size"   *> pWs *> pInteger)) <|>
    try (MemoryOptionOffset <$> (string "offset" *> pWs *> pInteger))

-- Cells
pCellStmt :: Parser CellStmt
pCellStmt = do
    _ <- string "cell"
    _ <- pWs
    cellId <- CellId <$> pId
    _ <- pWs
    cellType <- CellType <$> pId
    _ <- pEol
    return $ CellStmt cellId cellType

-- Processes
-- Switches
-- Syncs




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