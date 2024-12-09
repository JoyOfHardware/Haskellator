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
    Process(..), ProcStmt(..), ProcessBody(..), AssignStmt(..),
    DestSigSpec(..), SrcSigSpec(..),

    -- Switches
    Switch(..), SwitchStmt(..), Case(..), CaseStmt(..), Compare(..),
    CaseBodyVariants(..), CaseBody(..),

    -- Syncs
    Sync(..), SyncStmt(..), SyncType(..), UpdateStmt(..)
    )
import RTLILParser.Primitives(
    pWs
   ,pNonWs
   ,pMaybeWs
   ,pEol
   ,pOctal
   ,pEscapedChar
   ,pEolAndAdvanceToNextNonWs
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
    char '\''
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
pAutoIdxStmt = AutoIdxStmt
    <$> (string "autoidx" *> pWs *>
         pInteger <* pEolAndAdvanceToNextNonWs)

-- Module
pModuleStmt :: Parser Id
pModuleStmt = string "module" *> pWs *> pId <*
              pEolAndAdvanceToNextNonWs

pParamStmt :: Parser ParamStmt
pParamStmt = ParamStmt
    <$> (string "parameter" *> pWs *> pId <* pWs)
    <*> optionMaybe pConstant
    <*  pEolAndAdvanceToNextNonWs

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
    <*  pEolAndAdvanceToNextNonWs

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
    char '{' <* pWs
    sigspecs    <- pSigSpec `sepBy` pWs
    pWs <* char '}'
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
    <*  pEolAndAdvanceToNextNonWs

-- Wires
pWire :: Parser Wire
pWire = do
    attrs <- many pAttrStmt
    wireStmt <- pWireStmt
    return $ Wire wireStmt attrs

pWireStmt :: Parser WireStmt
pWireStmt = do
    string "wire" <* pWs
    options <- many pWireOption <* pWs
    wireId  <- WireId <$> pId <* pEolAndAdvanceToNextNonWs
    return $ WireStmt wireId options

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
pMemoryStmt = do
    (string "memory" <* pWs)
    options     <- (many pMemoryOption <* pWs)
    memoryId    <- MemoryID <$> pId
    pEolAndAdvanceToNextNonWs
    return $ MemoryStmt memoryId options

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
    cellBodyStmts   <- many pCellBodyStmt <* pCellEndStmt
    return $ Cell cellStmt attrStmts cellBodyStmts

pCellStmt :: Parser CellStmt
pCellStmt = do
    string "cell"
    pWs
    cellType <- CellType <$> pId
    pWs
    cellId <- CellId <$> pId
    pEolAndAdvanceToNextNonWs
    return $ CellStmt cellId cellType

pCellBodyStmt :: Parser CellBodyStmt
pCellBodyStmt = pCellBodyParameter <|> pCellBodyConnect

pParameterSign :: Parser ParameterSign
pParameterSign =
    (Signed     <$ string "signed") <|>
    (Real       <$ string "real")

pCellBodyParameter :: Parser CellBodyStmt
pCellBodyParameter = do
    string "parameter" <* pWs
    sign    <- optionMaybe pParameterSign <* pMaybeWs
    id      <- pId
    const   <- pConstant <* pEolAndAdvanceToNextNonWs
    return $ CellBodyParameter sign id const

pCellBodyConnect :: Parser CellBodyStmt
pCellBodyConnect = do
    string "connect" <* pWs
    id      <- pId <* pWs
    sigSpec <- pSigSpec <* pEolAndAdvanceToNextNonWs
    return  $ CellConnect id sigSpec

pCellEndStmt :: Parser ()
pCellEndStmt = void (string "end" <* pEolAndAdvanceToNextNonWs)

-- Processes
pProcess :: Parser Process
pProcess = do
    attrs       <- many pAttrStmt
    procStmt    <- pProcStmt
    processBody <- pProcessBody
    pProcEndStmt
    return $ Process procStmt attrs processBody

pProcStmt :: Parser ProcStmt
pProcStmt = ProcStmt
    <$> (string "process" *> pWs *> pId)
    <*  pEolAndAdvanceToNextNonWs

pProcessBody :: Parser ProcessBody
pProcessBody = do
    -- Since the pAssignStmt parser begins with "assign" and the pSwitch
    -- parser technically begins with "attribute", these both starting
    -- with the character 'a', we need to be able to rewind failed
    -- attempts for `pAssignStmt` and `pSwitch` parsers as the first
    -- character being an 'a' would have been consumed.
    assignStmtsBefore   <- many $ try pAssignStmt
    switch              <- optionMaybe $ try pSwitch
    assignStmtsAfter    <- many pAssignStmt
    syncs               <- many pSync
    return $ ProcessBody assignStmtsBefore switch assignStmtsAfter syncs

pAssignStmt :: Parser AssignStmt
pAssignStmt = AssignStmt
    <$> (string "assign" *> pWs *> pDestSigSpec)
    <*> (pWs *> pSrcSigSpec <* pEolAndAdvanceToNextNonWs)

pDestSigSpec :: Parser DestSigSpec
pDestSigSpec = DestSigSpec <$> pSigSpec

pSrcSigSpec :: Parser SrcSigSpec
pSrcSigSpec = SrcSigSpec <$> pSigSpec

pProcEndStmt :: Parser ()
pProcEndStmt = void (string "end" <* pEolAndAdvanceToNextNonWs)

-- Switches
pSwitch :: Parser Switch
pSwitch = Switch
    <$> pSwitchStmt
    <*> (many pCase <* pSwitchEndStmt)

pSwitchStmt :: Parser SwitchStmt
pSwitchStmt = do
    attrs   <- many pAttrStmt
    string "switch"  <* pWs
    sigspec <- pSigSpec <* pEolAndAdvanceToNextNonWs
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
        <* pEolAndAdvanceToNextNonWs)

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
pSwitchEndStmt = void (string "end" *> pEolAndAdvanceToNextNonWs)

-- Syncs
pSync :: Parser Sync
pSync = Sync
    <$> pSyncStmt
    <*> many pUpdateStmt

pSyncStmt :: Parser SyncStmt
pSyncStmt =  pKeywordSync *>
                pSigSpecPredicatedSyncStmt <|>
                pNonSigSpecPredicatedSyncStmt
                where pKeywordSync = string "sync" *> pWs

pSigSpecPredicatedSyncStmt :: Parser SyncStmt
pSigSpecPredicatedSyncStmt = do
    syncType    <-  pSyncType <* pWs
    sigSpec     <-  pSigSpec  <* pEolAndAdvanceToNextNonWs
    return $ SigSpecPredicated sigSpec syncType

pNonSigSpecPredicatedSyncStmt :: Parser SyncStmt
pNonSigSpecPredicatedSyncStmt =
    keyword <* pEolAndAdvanceToNextNonWs
    where keyword =
            (Global <$ string "global"  ) <|>
            (Init   <$ string "init"    ) <|>
            (Always <$ string "always"  )

pSyncType :: Parser SyncType
pSyncType =
    (Low        <$ string "low"     )   <|>
    (High       <$ string "high"    )   <|>
    (Posedge    <$ string "posedge" )   <|>
    (Negedge    <$ string "negedge" )   <|>
    (Edge       <$ string "edge"    )

pUpdateStmt :: Parser UpdateStmt
pUpdateStmt = UpdateStmt
    <$> (string "update" *> pWs *> pDestSigSpec)
    <*> (pWs *> pSrcSigSpec <* pEolAndAdvanceToNextNonWs)

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