module RTLILParser.AST(
    AutoIdxStmt(..)    ,ParamStmt(..)  ,AutogenId(..)
   ,Constant(..)       ,CellStmt(..)   ,PublicId(..)
   ,AttrStmt(..)       ,Value(..)      ,Id(..)
   ,CellId(..)         ,CellType(..)   ,WireId(..)
   ,SigSpec(..)        ,Slice(..)      ,ConnStmt(..)
   ,WireOption(..)     ,WireStmt(..)   ,Wire(..)
   ,MemoryOption(..)   ,MemoryStmt(..) ,Memory(..)
   ,MemoryID(..)
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
    , value :: Int
    }
    deriving (Show)

-- strings
-- comments
-- file

-- Autoindex statements
data AutoIdxStmt    = AutoIdxStmt   Int     deriving (Show)

-- Module
data ParamStmt = ParamStmt
    { paramId       :: Id
    , paramConstant :: Maybe Constant
    }
    deriving (Show)
data Constant = ConstantValue   Value
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
data CellStmt       = CellStmt      CellId CellType  deriving (Show)
data CellId         = CellId        Id deriving (Show)
data CellType       = CellType      Id deriving (Show)

-- Processes
-- Switches
-- Syncs