module RTLILParser.Parser(a, val) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Util(binaryStringToInt)
import RTLILParser.AST (
    -- Identifiers
    Id(..), PublicId(..), AutogenId(..),

    -- Values
    Value(..),

    -- Autoindex statements
    AutoIdxStmt(..),

    -- Module
    ParamStmt(..), Constant(..),

    -- Attribute statements
    AttrStmt(..),

    -- Signal Specifications
    SigSpec(..), Slice(..),

    -- Connections
    ConnStmt(..),

    -- Wires
    Wire(..), WireStmt(..), WireId(..), WireOption(..),

    -- Memories
    Memory(..), MemoryStmt(..), MemoryID(..), MemoryOption(..),

    -- Cells
    Cell(..), CellStmt(..), CellId(..), CellType(..), ParameterSign(..),
    CellBodyStmt(..),

    -- Processes
    DestSigSpec(..), SrcSigSpec(..), AssignStmt(..),

    -- Switches
    Switch(..), SwitchStmt(..), Case(..), CaseStmt(..), Compare(..),
    CaseBodyVariants(..), CaseBody(..)
    )
import RTLILParser.Primitives(
    pWs
   ,pNonWs
   ,pMaybeWs
   ,pEol
   ,pOctal
   ,pEscapedChar
    )
import Text.Parsec.Token (GenLanguageDef(caseSensitive))
import GHC.IO.Handle.Types (Handle__(Handle__))

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
pAutoIdxStmt = AutoIdxStmt <$> (string "autoidx" *> pWs *> pInteger <* pEol <* pMaybeWs)

-- Module
pModuleStmt :: Parser Id
pModuleStmt = string "module" *> pWs *> pId <* pEol <* pMaybeWs

pParamStmt :: Parser ParamStmt
pParamStmt = ParamStmt
    <$> (string "parameter" *> pWs *> pId <* pWs)
    <*> optionMaybe pConstant
    <*  pEol <*  pMaybeWs

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
    <*  pEol <* pMaybeWs

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
    <$> (pMaybeWs *> char '[' *> pMaybeWs *> pInteger <* pMaybeWs)
    <*> (optionMaybe (char ':' *> pInteger) <* pMaybeWs <* char ']')

-- Connections
pConnStmt :: Parser ConnStmt
pConnStmt = ConnStmt
    <$> (string "connect" *> pWs *> pSigSpec)
    <*> (pWs *> pSigSpec)
    <*  pEol <* pMaybeWs

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
    <*  pEol <* pMaybeWs

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
    <*  pEol <* pMaybeWs

pMemoryOption :: Parser MemoryOption
pMemoryOption =
    try (MemoryOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|>
    try (MemoryOptionSize   <$> (string "size"   *> pWs *> pInteger)) <|>
    try (MemoryOptionOffset <$> (string "offset" *> pWs *> pInteger))

-- Cells
pCell :: Parser Cell
pCell = do
    attrStmts       <- many pAttrStmt
    cellStmt        <- pCellStmt
    cellBodyStmts   <- many pCellBodyStmt
    return $ Cell cellStmt attrStmts cellBodyStmts

pCellStmt :: Parser CellStmt
pCellStmt = do
    _ <- string "cell"
    _ <- pWs
    cellType <- CellType <$> pId
    _ <- pWs
    cellId <- CellId <$> pId
    _ <- pEol <* pMaybeWs
    return $ CellStmt cellId cellType

pCellBodyStmt :: Parser CellBodyStmt
pCellBodyStmt = pCellBodyParameter <|> pCellBodyConnect

pParameterSign :: Parser ParameterSign
pParameterSign =
    (Signed     <$ string "signed") <|>
    (Real       <$ string "real")

pCellBodyParameter :: Parser CellBodyStmt
pCellBodyParameter = do
    _       <- string "parameter" <* pWs
    sign    <- optionMaybe pParameterSign <* pMaybeWs
    id      <- pId
    const   <- pConstant <* pEol <* pMaybeWs
    return $ CellBodyParameter sign id const

pCellBodyConnect :: Parser CellBodyStmt
pCellBodyConnect = do
    _       <- string "connect" <* pWs
    id      <- pId <* pWs
    sigSpec <- pSigSpec <* pEol <* pMaybeWs
    return  $ CellConnect id sigSpec

-- Processes
pDestSigSpec :: Parser DestSigSpec
pDestSigSpec = DestSigSpec <$> pSigSpec

pSrcSigSpec :: Parser SrcSigSpec
pSrcSigSpec = SrcSigSpec <$> pSigSpec

pAssignStmt :: Parser AssignStmt
pAssignStmt = AssignStmt
    <$> (string "assign" *> pWs *> pDestSigSpec)
    <*> (pWs *> pSrcSigSpec <* pEol <* pMaybeWs)

-- Switches
-- - [ ] <switch>            ::= <switch-stmt> <case>* <switch-end-stmt>
-- - [ ] <switch-stmt>       ::= <attr-stmt>* switch <sigspec> <eol>
-- - [ ] <case>              ::= <attr-stmt>* <case-stmt> <case-body>
-- - [x] <case-stmt>         ::= case <compare>? <eol>
-- - [x] <compare>           ::= <sigspec> (, <sigspec>)*
-- - [ ] <case-body>         ::= (<switch> | <assign-stmt>)*
-- - [ ] <switch-end-stmt>   ::= end <eol>

pSwitch :: Parser Switch
pSwitch = Switch
    <$> pSwitchStmt
    <*> (many pCase <* pSwitchEndStmt)

pSwitchStmt :: Parser SwitchStmt
pSwitchStmt = do
    attrs   <- many pAttrStmt
    _       <- string "switch"  <* pWs
    sigspec <- pSigSpec <* pEol <* pMaybeWs
    return $ SwitchStmt sigspec attrs

pCase :: Parser Case
pCase = Case
    <$> pCaseStmt
    <*> many pAttrStmt
    <*> pCaseBody

pCaseStmt :: Parser CaseStmt
pCaseStmt = CaseStmt
    <$> (
        string "case" *> pWs 
        *> optionMaybe pCompare 
        <* pEol <* pMaybeWs)

pCompare :: Parser Compare
pCompare = Compare
    <$> pSigSpec `sepBy` (pMaybeWs *> char ',' *> pMaybeWs)

pCaseBody :: Parser CaseBody
pCaseBody = CaseBody <$> many pCaseBodyVariant

pCaseBodyVariant :: Parser CaseBodyVariants
pCaseBodyVariant =
    try (CaseBodySwitchVariant <$> pSwitch    ) <|>
        (CaseBodyAssignVariant <$> pAssignStmt)

pSwitchEndStmt :: Parser ()
pSwitchEndStmt = void (string "end" *> pEol *> pMaybeWs)

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