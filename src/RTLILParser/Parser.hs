module RTLILParser.Parser(
    RTLILParser.Parser.runParser,
    a,
    val) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Util(binaryStringToInt)
import RTLILParser.AST (
    -- Identifiers
    Id(..), PublicId(..), AutogenId(..),

    -- Values
    Value(..),

    -- File
    File(..),

    -- Autoindex statements
    AutoIdxStmt(..),

    -- Module
    Module(..), ModuleStmt(..), ModuleBody(..),
    ModuleBodyVariant(..), ParamStmt(..), Constant(..),

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
   ,advanceToNextToken
   ,advanceToFirstToken
    )

-- taken from: https://yosyshq.readthedocs.io/projects/yosys/en/0.47/appendix/rtlil_text.html
-- parsers below are split int sections from the above link

runParser :: String -> SourceName -> File
runParser str filename = case parse pFile filename str of
    Left err -> error $ show err
    Right val -> val

-- identifiers
pId :: Parser Id
pId = p <?> name where
    name = "Id"
    p =
        Public  <$> pPublicId
        <|> Autogen <$> pAutogenId

pPublicId   :: Parser PublicId
pPublicId   = (PublicId <$> (char '\\' *> many1 pNonWs))
    <?> "PublicId"

pAutogenId  :: Parser AutogenId
pAutogenId  = (AutogenId <$> (char '$' *> many1 pNonWs))
           <?> "AutogenId"

-- values
pValue :: Parser Value
pValue = p <?> name where
    name = "Value"
    p =
        do
        width <- many1 pDecimalDigit
        char '\''
        binaryValue <- many pBinaryDigit
        return $ Value (read width) binaryValue

pDecimalDigit :: Parser Char
pDecimalDigit = oneOf "0123456789" <?> "DecimalDigit"

-- Haskellator simulation will likely only support 0/1 and 
-- don't cares. We can however be permissive when parsing, 
-- and raise errors later during validation.
pBinaryDigit :: Parser Char
pBinaryDigit = oneOf "01xzm-" <?> "BinaryDigit"

pInteger :: Parser Int
pInteger = p <?> name where
    name = "Integer"
    p =
        do
        sign <- optionMaybe (char '-')
        digits <- many1 pDecimalDigit
        let value = read digits
        return $ case sign of
            Just _  -> -value
            Nothing ->  value

-- strings
pString :: Parser String
pString = p <?> name where
    name = "String"
    p =
        between delimiter delimiter parseString
        where
            delimiter = char '"'
            parseString = many (pEscapedChar <|> noneOf "\\\"")

-- file
pFile :: Parser File
pFile = p <?> name where
    name = "File"
    p =
        File
        <$> (advanceToFirstToken *> optionMaybe (try pAutoIdxStmt))
        <*> many pModule

-- Autoindex statements
pAutoIdxStmt :: Parser AutoIdxStmt
pAutoIdxStmt = p <?> name where
    name = "AutoIdxStmt"
    p =
        AutoIdxStmt
        <$> (string "autoidx" *> pWs *>
            pInteger <* advanceToNextToken)

-- Module
pModule :: Parser Module
pModule = p <?> name where
    name = "Module"
    p =
        do
        attrs       <- many (try pAttrStmt)
        moduleStmt  <- pModuleStmt
        moduleBody  <- pModuleBody
        pModuleEndStmt
        -- return $ Module moduleStmt attrs $ModuleBody []
        return $ Module moduleStmt attrs moduleBody

pModuleStmt :: Parser ModuleStmt
pModuleStmt = p <?> name where
    name = "ModuleStmt"
    p =
        ModuleStmt
        <$> (string "module" *> pWs *>
            pId <* advanceToNextToken)

pModuleBody :: Parser ModuleBody
pModuleBody = p <?> name where
    name = "ModuleBody"
    p =
        ModuleBody
        <$> many (try pModuleBodyVariant)

pModuleBodyVariant :: Parser ModuleBodyVariant
pModuleBodyVariant = p <?> name where
    name = "ModuleBodyVariant"
    -- `pWire`, `pMemory`, `pCell`, `pProcess` all
    -- start by parsing attribute statements, so we
    -- need backtracking since we can't determin which
    -- parser will succeed based on the first character
    -- we encounter alone. `pParamStmt` technically doesn't
    -- need to be prefixed by `try`, so that is a stylistic
    -- choice.
    p =
        try (ModuleBodyParamStmt    <$> pParamStmt) <|>
        try (ModuleBodyWire         <$> pWire     ) <|>
        try (ModuleBodyMemory       <$> pMemory   ) <|>
        try (ModuleBodyCell         <$> pCell     ) <|>
        try (ModuleBodyProcess      <$> pProcess  ) <|>
        try (ModuleBodyConnStmt     <$> pConnStmt )

pParamStmt :: Parser ParamStmt
pParamStmt = p <?> name where
    name = "ParamStmt"
    p =
        ParamStmt
        <$> (string "parameter" *> pWs *> pId <* pWs)
        <*> optionMaybe pConstant
        <*  advanceToNextToken

pConstant :: Parser Constant
pConstant = p <?> name where
    name = "Constant"
    p =
        try (ConstantValue      <$> pValue  )
        <|> (ConstantInteger    <$> pInteger)
        <|> (ConstantString     <$> pString )

pModuleEndStmt :: Parser ()
pModuleEndStmt = p <?> name where
    name = "ModuleEndStmt"
    p =
        void (string "end")

-- Attribute statements
pAttrStmt :: Parser AttrStmt
pAttrStmt = p <?> name where
    name = "AttrStmt"
    p =
        AttrStmt
        <$> (string "attribute" *> pWs *> pId)
        <*> (pWs *> pConstant)
        <*  advanceToNextToken

-- Signal Specifications
pSigSpec :: Parser SigSpec
pSigSpec = p <?> name where
    name = "SigSpec"
    p =
        do
        baseSigSpec <- (SigSpecConstant <$> pConstant)
                    <|>
                    (SigSpecWireId   <$> pWireId)
                    <|>
                    pSigSpecConcat
        applySlices baseSigSpec

pSigSpecConcat :: Parser SigSpec
pSigSpecConcat = p <?> name where
    name = "SigSpecConcat"
    p =
        do
        char '{' <* pMaybeWs
        sigspecs    <- many $ pSigSpec <* pWs
        char '}'
        return $ SigSpecConcat sigspecs

applySlices :: SigSpec -> Parser SigSpec
applySlices base = p <?> name where
    name = "ApplySlices"
    p =
        do
        maybeSlice <- optionMaybe $ try pSlice
        case maybeSlice of
            Nothing -> return base
            Just slice -> applySlices (SigSpecSlice base slice)

pSlice :: Parser Slice
pSlice = p <?> name where
    name = "Slice"
    p =
        Slice
        <$> (pWs *> char '[' *> pMaybeWs *> pInteger <* pMaybeWs)
        <*> (optionMaybe (char ':' *> pInteger) <* pMaybeWs <* char ']')

-- Connections
pConnStmt :: Parser ConnStmt
pConnStmt = p <?> name where
    name = "ConnStmt"
    p =
        ConnStmt
        <$> (string "connect" *> pWs *> pSigSpec)
        <*> (pWs *> pSigSpec)
        <*  advanceToNextToken

-- Wires
pWire :: Parser Wire
pWire = p <?> name where
    name = "Wire"
    p = do
        attrs <- many pAttrStmt
        wireStmt <- pWireStmt
        return $ Wire wireStmt attrs

pWireStmt :: Parser WireStmt
pWireStmt = p <?> name where
    name = "WireStmt"
    p = do
        string "wire" <* pWs
        options <- many (pWireOption <* pWs)
        wireId  <- WireId <$> pId <* advanceToNextToken
        return $ WireStmt wireId options

pWireId :: Parser WireId
pWireId = p <?> name where
    name = "WireId"
    p =
        WireId <$> pId

pWireOption :: Parser WireOption
pWireOption = p <?> name where
    name = "WireOption"
    p =
        -- We technically don't need the first `try` below so this is a
        -- stylistic choice. The other `try` statements are needed.
        try (WireOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|>
        try (WireOptionOffset <$> (string "offset" *> pWs *> pInteger)) <|>
        try (WireOptionInput  <$> (string "input"  *> pWs *> pInteger)) <|>
        try (WireOptionOutput <$> (string "output" *> pWs *> pInteger)) <|>
        try (WireOptionInout  <$> (string "inout"  *> pWs *> pInteger)) <|>
        try (string "upto"      *> return WireOptionUpto)               <|>
        try (string "signed"    *> return WireOptionSigned)

-- Memories
pMemory :: Parser Memory
pMemory = p <?> name where
    name = "Memory"
    p =
        do
        attrs <- many pAttrStmt
        memoryStmt <- pMemoryStmt
        return $ Memory memoryStmt attrs

pMemoryStmt :: Parser MemoryStmt
pMemoryStmt = p <?> name where
    name = "MemoryStmt"
    p =
        do
        (string "memory" <* pWs)
        options     <- (many pMemoryOption <* pMaybeWs)
        memoryId    <- MemoryID <$> pId
        advanceToNextToken
        return $ MemoryStmt memoryId options

pMemoryOption :: Parser MemoryOption
pMemoryOption = p <?> name where
    name = "MemoryOption"
    p =
        try (MemoryOptionWidth  <$> (string "width"  *> pWs *> pInteger)) <|>
        try (MemoryOptionSize   <$> (string "size"   *> pWs *> pInteger)) <|>
        try (MemoryOptionOffset <$> (string "offset" *> pWs *> pInteger))

-- Cells
pCell :: Parser Cell
pCell = p <?> name where
    name = "Cell"
    p =
        do
        attrStmts       <- many pAttrStmt
        cellStmt        <- pCellStmt
        cellBodyStmts   <- many pCellBodyStmt <* pCellEndStmt
        return $ Cell cellStmt attrStmts cellBodyStmts

pCellStmt :: Parser CellStmt
pCellStmt = p <?> name where
    name = "CellStmt"
    p =
        do
        string "cell"
        pWs
        cellType <- CellType <$> pId
        pWs
        cellId <- CellId <$> pId
        advanceToNextToken
        return $ CellStmt cellId cellType

pCellBodyStmt :: Parser CellBodyStmt
pCellBodyStmt = p <?> name where
    name = "CellBodyStmt"
    p =
        pCellBodyParameter <|> pCellBodyConnect

pParameterSign :: Parser ParameterSign
pParameterSign = p <?> name where
    name = "ParameterSign"
    p =
        (Signed     <$ string "signed") <|>
        (Real       <$ string "real")

pCellBodyParameter :: Parser CellBodyStmt
pCellBodyParameter = p <?> name where
    name = "CellBodyParameter"
    p =
        do
        string "parameter" <* pWs
        sign    <- optionMaybe pParameterSign
        pMaybeWs
        id      <- pId <* pWs
        const   <- pConstant <* advanceToNextToken
        return $ CellBodyParameter sign id const

pCellBodyConnect :: Parser CellBodyStmt
pCellBodyConnect = p <?> name where
    name = "CellBodyConnect"
    p =
        do
        string "connect" <* pWs
        id      <- pId <* pWs
        sigSpec <- pSigSpec <* advanceToNextToken
        return  $ CellConnect id sigSpec

pCellEndStmt :: Parser ()
pCellEndStmt = void (string "end" <* advanceToNextToken)
            <?> "CellEndStmt"

-- Processes
pProcess :: Parser Process
pProcess = p <?> name where
    name = "Process"
    p =
        do
        attrs       <- many pAttrStmt
        procStmt    <- pProcStmt
        processBody <- pProcessBody
        pProcEndStmt
        return $ Process procStmt attrs processBody

pProcStmt :: Parser ProcStmt
pProcStmt = p <?> name where
    name = "ProcStmt"
    p =
        ProcStmt
        <$> (string "process" *> pWs *> pId)
        <*  advanceToNextToken

pProcessBody :: Parser ProcessBody
pProcessBody = p <?> name where
    name = "ProcessBody"
    p =
        do
        -- Since the pAssignStmt parser begins with "assign" and the pSwitch
        -- parser technically begins with "attribute", these both starting
        -- with the character 'a', we need to be able to rewind failed
        -- attempts for `pAssignStmt` and `pSwitch` parsers as the first
        -- character being an 'a' would have been consumed.
        assignStmts <- many $ try pAssignStmt
        switch      <- many $ try pSwitch
        syncs       <- many pSync
        return $ ProcessBody assignStmts switch syncs

pAssignStmt :: Parser AssignStmt
pAssignStmt = p <?> name where
    name = "AssignStmt"
    p =
        AssignStmt
        <$> (string "assign" *> pWs *> pDestSigSpec)
        <*> (pWs *> pSrcSigSpec <* advanceToNextToken)

pDestSigSpec :: Parser DestSigSpec
pDestSigSpec = (DestSigSpec <$> pSigSpec) <?> "DestSigSpec"

pSrcSigSpec :: Parser SrcSigSpec
pSrcSigSpec = (SrcSigSpec <$> pSigSpec) <?> "SrcSigSpec"

pProcEndStmt :: Parser ()
pProcEndStmt = void (string "end" <* advanceToNextToken)
            <?> "ProcEndStmt"

-- Switches
pSwitch :: Parser Switch
pSwitch = p <?> name where
    name = "Switch"
    p =
        Switch
        <$> pSwitchStmt
        <*> many pCase <* pSwitchEndStmt

pSwitchStmt :: Parser SwitchStmt
pSwitchStmt =  p <?> name where
    name = "SwitchStmt"
    p =
        do
        attrs   <- many pAttrStmt
        string "switch"  <* pWs
        sigspec <- pSigSpec <* advanceToNextToken
        return $ SwitchStmt sigspec attrs

pCase :: Parser Case
pCase = p <?> name where
    name = "Case"
    p =
        do
        attrs <- many pAttrStmt
        caseStmt <- pCaseStmt
        caseBody <- pCaseBody
        return $ Case caseStmt attrs caseBody

pCaseStmt :: Parser CaseStmt
pCaseStmt = p <?> name where
    name = "CaseStmt"
    p =
        CaseStmt
        <$> (
            string "case" *> pMaybeWs
            *> optionMaybe pCompare
            <* advanceToNextToken)

pCompare :: Parser Compare
pCompare = p <?> name where
    name = "Compare"
    p =
        Compare
        <$> pSigSpec `sepBy` (pMaybeWs *> char ',' *> pMaybeWs)

pCaseBody :: Parser CaseBody
pCaseBody = p <?> name where
    name = "CaseBody"
    p =
        CaseBody
        <$> many (try pAssignStmt)
        <*> many (try pSwitch)

pCaseBodyVariant :: Parser CaseBodyVariants
pCaseBodyVariant = p <?> name where
    name = "CaseBodyVariant"
    p =
        try (CaseBodySwitchVariant <$> pSwitch    ) <|>
        try (CaseBodyAssignVariant <$> pAssignStmt)

pSwitchEndStmt :: Parser ()
pSwitchEndStmt = void (string "end" *> advanceToNextToken)
              <?> "SwitchEndStmt"

-- Syncs
pSync :: Parser Sync
pSync = p <?> name where
    name = "Sync"
    p =
        Sync
        <$> pSyncStmt
        <*> many pUpdateStmt

pSyncStmt :: Parser SyncStmt
pSyncStmt =  p <?> name where
    name = "SyncStmt"
    p =
        pKeywordSync *>
                    pSigSpecPredicatedSyncStmt <|>
                    pNonSigSpecPredicatedSyncStmt
                    where pKeywordSync = string "sync" *> pWs

pSigSpecPredicatedSyncStmt :: Parser SyncStmt
pSigSpecPredicatedSyncStmt = p <?> name where
    name = "SigSpecPredicatedSyncStmt"
    p =
        do
        syncType    <-  pSyncType <* pWs
        sigSpec     <-  pSigSpec  <* advanceToNextToken
        return $ SigSpecPredicated sigSpec syncType

pNonSigSpecPredicatedSyncStmt :: Parser SyncStmt
pNonSigSpecPredicatedSyncStmt = p <?> name where
    name = "NonSigSpecPredicatedSyncStmt"
    p =
        keyword <* advanceToNextToken
        where keyword =
                (Global <$ string "global"  ) <|>
                (Init   <$ string "init"    ) <|>
                (Always <$ string "always"  )

pSyncType :: Parser SyncType
pSyncType = p <?> name where
    name = "SyncType"
    p =
        (Low        <$ string "low"     )   <|>
        (High       <$ string "high"    )   <|>
        (Posedge    <$ string "posedge" )   <|>
        (Negedge    <$ string "negedge" )   <|>
        (Edge       <$ string "edge"    )

pUpdateStmt :: Parser UpdateStmt
pUpdateStmt = p <?> name where
    name = "UpdateStmt"
    p =
        UpdateStmt
        <$> (string "update" *> pWs *> pDestSigSpec)
        <*> (pWs *> pSrcSigSpec <* advanceToNextToken)

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