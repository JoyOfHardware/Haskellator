{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
module RTLIL.Syntax where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (..), hsep, vsep, nest, parens, (<+>), Doc, colon, tupled)

type AutoIdx = Integer
type UpTo = Bool
type Signed = Bool
type Ident = Text
type WireIdent = Text
type Width = Natural
type Offset = Natural
type PortIdx = Int
type Size = Natural
type CellIdent = Text
type CellType = Text

data File = File (Maybe AutoIdx) [Module]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Module = Module [Attr] Ident [Stmt]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Attr = Attr Ident Lit
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Lit = LitText Text | LitInt Integer
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Stmt = Param Ident (Maybe Lit)
          | Wire [Attr] Width Offset (Maybe PortDecl) UpTo Signed WireIdent
          | Memory Width Offset Size Ident
          | Cell [Attr] [CellStmt] CellType CellIdent
          | Proc
          | Connect SigSpec SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data PortDecl = PortDecl Direction PortIdx
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data Direction = Input | Output | InOut
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data CellStmt = CellParam (Maybe CellParamSpec) Ident Lit
              | CellConnect Ident SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data CellParamSpec = CPSigned | CPReal
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

data SigSpec = SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

