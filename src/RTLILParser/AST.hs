module RTLILParser.AST (
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
    Sync(..), SyncStmt(..), SyncType(..), UpdateStmtVariants(..)


) where

import Text.Read (Lexeme(Ident))
import Data.Functor.Contravariant (Contravariant)
import GHC.RTS.Flags (DoCostCentres(CostCentresAll))

-- taken from: https://yosyshq.readthedocs.io/projects/yosys/en/0.47/appendix/rtlil_text.html
-- types below organized accordingly

-- identifiers
data Id             = Public    PublicId
                    | Autogen   AutogenId
                    deriving (Show)
data PublicId       = PublicId      String  deriving (Show)
data AutogenId      = AutogenId     String  deriving (Show)

-- values
data Value = Value
    { width :: Int
    , binaryValue :: String
    }
    deriving (Show)

-- strings
-- comments
-- file
data File           = File (Maybe AutoIdxStmt) [Module] deriving (Show)

-- Autoindex statements
data AutoIdxStmt    = AutoIdxStmt   Int     deriving (Show)

-- Module
data Module         = Module ModuleStmt [AttrStmt] ModuleBody deriving (Show)
data ModuleStmt     = ModuleStmt Id deriving (Show)
data ModuleBody     = ModuleBody [ModuleBodyVariant] deriving (Show)
data ModuleBodyVariant  = ModuleBodyParamStmt   ParamStmt
                        | ModuleBodyWire        Wire
                        | ModuleBodyMemory      Memory
                        | ModuleBodyCell        Cell
                        | ModuleBodyProcess     Process
                        | ModuleBodyConnStmt    ConnStmt
                        deriving (Show)
data ParamStmt      = ParamStmt Id (Maybe Constant) deriving (Show)
data Constant       = ConstantValue   Value
                    | ConstantInteger Int
                    | ConstantString  String
                    deriving (Show)

-- Attribute statements
data AttrStmt       = AttrStmt      Id Constant deriving (Show)

-- Signal Specifications
data SigSpec        = SigSpecConstant   Constant
                    | SigSpecWireId     WireId
                    | SigSpecSlice      SigSpec Slice
                    | SigSpecConcat     [SigSpec]
                    deriving (Show)
data Slice          = Slice         Int (Maybe Int) deriving (Show)

-- Connections
data ConnStmt       = ConnStmt      SigSpec SigSpec deriving (Show)

-- Wires
data Wire           = Wire          WireStmt [AttrStmt] deriving (Show)
data WireStmt       = WireStmt      WireId [WireOption] deriving (Show)
data WireId         = WireId    Id deriving (Show)
data WireOption     = WireOptionWidth   Int
                    | WireOptionOffset  Int
                    | WireOptionInput   Int
                    | WireOptionOutput  Int
                    | WireOptionInout   Int 
                    | WireOptionUpto
                    | WireOptionSigned
                    deriving (Show)

-- Memories
data Memory         = Memory        MemoryStmt [AttrStmt] deriving (Show)
data MemoryID       = MemoryID  Id deriving (Show)
data MemoryStmt     = MemoryStmt    MemoryID [MemoryOption] deriving (Show)
data MemoryOption   = MemoryOptionWidth     Int
                    | MemoryOptionSize      Int
                    | MemoryOptionOffset    Int
                    deriving (Show)

-- Cells
data Cell           = Cell          CellStmt [AttrStmt] [CellBodyStmt]
                      deriving (Show)
data CellStmt       = CellStmt      CellId CellType  deriving (Show)
data CellId         = CellId        Id deriving (Show)
data CellType       = CellType      Id deriving (Show)
data ParameterSign  = Signed | Real deriving (Show)
data CellBodyStmt   = CellBodyParameter 
                        (Maybe ParameterSign)
                        Id
                        Constant
                    | CellConnect Id SigSpec
                    deriving (Show)

-- Processes
data Process        = Process ProcStmt [AttrStmt] ProcessBody
                      deriving (Show)
data ProcStmt       = ProcStmt Id deriving (Show)
data ProcessBody    = ProcessBody 
                        [AssignStmt]
                        [Switch]
                        [Sync] 
                    deriving (Show)
data AssignStmt     = AssignStmt  DestSigSpec SrcSigSpec
                      deriving (Show)
data DestSigSpec    = DestSigSpec SigSpec  deriving (Show)
data SrcSigSpec     = SrcSigSpec  SigSpec  deriving (Show)

-- Switches
data Switch         = Switch SwitchStmt [Case]
                      deriving (Show)
data SwitchStmt     = SwitchStmt SigSpec [AttrStmt] deriving (Show)
data Case           = Case CaseStmt [AttrStmt] CaseBody
                      deriving (Show)
data CaseStmt       = CaseStmt (Maybe Compare)
                      deriving (Show)
data Compare        = Compare [SigSpec]
                      deriving (Show)
data CaseBodyVariants   = CaseBodySwitchVariant Switch
                        | CaseBodyAssignVariant AssignStmt
                        deriving (Show)
data CaseBody       = CaseBody [AssignStmt] [Switch] deriving (Show)

-- Syncs
data Sync           = Sync SyncStmt [UpdateStmtVariants] deriving (Show)
data SyncStmt       = SigSpecPredicated SigSpec SyncType
                    | Global
                    | Init
                    | Always
                    deriving (Show)
data SyncType       = Low
                    | High
                    | Posedge
                    | Negedge
                    | Edge
                    deriving (Show)
data UpdateStmtVariants = UpdateStmt DestSigSpec SrcSigSpec
                        | MemWrStmt  Id SigSpec SigSpec SigSpec Constant [AttrStmt]
                        deriving (Show)