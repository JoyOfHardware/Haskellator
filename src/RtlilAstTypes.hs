module RtlilAstTypes(
    PublicId(..),
    AutogenId(..),
    AutoIdxStmt(..),
    Id(..),
    Value(..)
    ) where
import Text.Read (Lexeme(Ident))

data PublicId       = PublicId      String  deriving (Show)
data AutogenId      = AutogenId     String  deriving (Show)
data Id             = Public    PublicId
                    | Autogen   AutogenId
                    deriving (Show)
data AutoIdxStmt    = AutoIdxStmt   Int     deriving (Show)
data Value = Value
    { width :: Int
    , value :: Int
    }
    deriving (Show)
data Constant = ConstantValue   Value
              | ConstantInteger Int
              | ConstantString  String
              deriving (Show)
