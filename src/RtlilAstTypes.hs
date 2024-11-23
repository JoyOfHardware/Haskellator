module RtlilAstTypes(
    PublicId(..),
    AutogenId(..),
    AutoIdxStmt(..),
    Value(..)
    ) where

data PublicId       = PublicId      String  deriving (Show)
data AutogenId      = AutogenId     String  deriving (Show)
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
