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
-- comments
-- file
-- Autoindex statements
-- Module
-- Attribute statements
-- Signal Specifications
-- Connections
-- Wires
-- Memories
-- Cells
-- Processes
-- Switches
-- Syncs

pWireId :: Parser WireId
pWireId = WireId <$> pId



pString :: Parser String
pString =
    between delimiter delimiter parseString
    where
        delimiter = char '"'
        parseString = many (pEscapedChar <|> noneOf "\\\"")

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

pConnStmt :: Parser ConnStmt
pConnStmt = ConnStmt
    <$> (string "connect" *> pWs *> pSigSpec)
    <*> (pWs *> pSigSpec)
    <*  pEol

pWireOption :: Parser WireOption
pWireOption =
    try (WireOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|> 
    try (WireOptionOffset <$> (string "offset" *> pWs *> pInteger)) <|> 
    try (WireOptionInput  <$> (string "input"  *> pWs *> pInteger)) <|> 
    try (WireOptionOutput <$> (string "output" *> pWs *> pInteger)) <|> 
    try (WireOptionInout  <$> (string "inout"  *> pWs *> pInteger)) <|> 
    (string "upto"      *> return WireOptionUpto)                   <|> 
    (string "signed"    *> return WireOptionSigned)

pWireStmt :: Parser WireStmt
pWireStmt = 
    WireStmt
    <$  string "wire" 
    <*  pWs
    <*> (WireId <$> pId)
    <*  pWs
    <*> many pWireOption
    <*  pEol

pWire :: Parser Wire
pWire = do
    attrs <- many pAttrStmt
    wireStmt <- pWireStmt
    return $ Wire wireStmt attrs

pMemoryOption :: Parser MemoryOption
pMemoryOption = 
    try (MemoryOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|> 
    try (MemoryOptionSize   <$> (string "size"   *> pWs *> pInteger)) <|> 
    try (MemoryOptionOffset <$> (string "offset" *> pWs *> pInteger))

pMemoryStmt :: Parser MemoryStmt
pMemoryStmt = 
    MemoryStmt
    <$  string "memory" 
    <*  pWs
    <*> (MemoryID <$> pId)
    <*  pWs
    <*> many pMemoryOption
    <*  pEol

pMemory :: Parser Memory
pMemory = do
    attrs <- many pAttrStmt
    memoryStmt <- pMemoryStmt
    return $ Memory memoryStmt attrs

-- <cell>              ::= <attr-stmt>* <cell-stmt> <cell-body-stmt>* <cell-end-stmt>
-- <cell-stmt>         ::= cell <cell-type> <cell-id> <eol>
-- <cell-id>           ::= <id>
-- <cell-type>         ::= <id>
-- <cell-body-stmt>    ::= parameter (signed | real)? <id> <constant> <eol>
--                      |  connect <id> <sigspec> <eol>
-- <cell-end-stmt>     ::= end <eol>

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