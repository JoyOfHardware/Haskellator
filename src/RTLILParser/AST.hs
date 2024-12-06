module RTLILParser.AST(
    AutoIdxStmt(..),    ParamStmt(..),  AutogenId(..),
    Constant(..),       CellStmt(..),   PublicId(..),
    AttrStmt(..),       Value(..),      Id(..),
    CellId(..),         CellType(..),   WireId(..),
    SigSpec(..),        Slice(..)
    ) where
import Text.Read (Lexeme(Ident))
import Data.Functor.Contravariant (Contravariant)
import GHC.RTS.Flags (DoCostCentres(CostCentresAll))

data PublicId       = PublicId      String  deriving (Show)
data AutogenId      = AutogenId     String  deriving (Show)
data Slice          = Slice         Int (Maybe Int) deriving (Show)
data Id             = Public    PublicId
                    | Autogen   AutogenId
                    deriving (Show)
data WireId         = WireId    Id
                    deriving (Show)
data AutoIdxStmt    = AutoIdxStmt   Int     deriving (Show)
data AttrStmt       = AttrStmt  Id Constant deriving (Show)
data CellStmt       = CellStmt  CellId CellType  deriving (Show)
data CellId         = CellId    Id deriving (Show)
data CellType       = CellType  Id deriving (Show)
data SigSpec        = SigSpecConstant   Constant
                    | SigSpecWireId     WireId
                    | SigSpecSlice      SigSpec Slice
                    | SigSpecConcat     [SigSpec]
                    deriving (Show)
data Value = Value
    { width :: Int
    , value :: Int
    }
    deriving (Show)
data Constant = ConstantValue   Value
              | ConstantInteger Int
              | ConstantString  String
              deriving (Show)
data ParamStmt = ParamStmt
    { paramId       :: Id
    , paramConstant :: Maybe Constant
    }
    deriving (Show)
