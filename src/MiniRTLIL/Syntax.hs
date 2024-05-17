{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module MiniRTLIL.Syntax
      ( AutoIdx, UpTo, Signed, Offset, PortIdx, Size, CellType
      , Ident (..), AttrStmt (..), Stmt (..), Block (..), Attr (..)
      , File (..), Module (..)
      , ModBody (..), ModAttrStmt (..), ModStmt (..), Cell (..)
      , WireOption (..)
      , CellBody (..), CellParamSpec (..)
      , Constant (..), SigSpec (..)
      ) where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (..), hsep, vsep, nest, (<+>), Doc, colon, braces, brackets, dquotes, squote)

type AutoIdx = Integer
type UpTo = Bool
type Signed = Bool
type Offset = Natural
type PortIdx = Int
type Size = Natural
type CellType = Ident

data Ident = Public Text
           | Autogen Text
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Ident where
      pretty = \ case
            Public x  -> text $ "\\" <> x
            Autogen x -> text $ "$" <> x

data AttrStmt a = AttrStmt [Stmt Attr] a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (AttrStmt a) where
      pretty (AttrStmt as x) = vsep $ map pretty as <> [pretty x]

data Stmt a = Stmt a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (Stmt a) where
      pretty (Stmt x) = vsep [pretty x]

data Block a = Block [Stmt Attr] a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (Block a) where
      pretty (Block as x) = vsep $ map pretty as <> [nest 2 $ pretty x, text "end"]

data Attr = Attr Ident Constant
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Attr where
      pretty (Attr x c) = text "attribute" <+> pretty x <+> pretty c

data File = File (Maybe (Stmt AutoIdx)) [Block Module]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty File where
      pretty (File idx body) = vsep $ ppAutoIdx idx <> map pretty body <> [mempty]

data Module = Module Ident [ModBody]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Module where
      pretty (Module x body) = vsep $ text "module" <+> pretty x : map pretty body

data Constant = ConstantValue Integer Text
              | ConstantInteger Integer
              | ConstantString Text
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Constant where
      pretty = \ case
            ConstantValue x b -> pretty x <> squote <> pretty b
            ConstantInteger x -> pretty x
            ConstantString x  -> dquotes $ pretty x

data ModBody = ModAttrStmt (AttrStmt ModAttrStmt)
             | ModStmt (Stmt ModStmt)
             | ModBlock (Block Cell)
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModBody where
      pretty = \ case
            ModAttrStmt s -> pretty s
            ModStmt s     -> pretty s
            ModBlock s    -> pretty s

data ModAttrStmt = Wire [WireOption] Ident
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModAttrStmt where
      pretty = \ case
            Wire [] x    -> text "wire" <+> pretty x
            Wire ops x   -> text "wire" <+> hsep (map pretty ops) <+> pretty x

data ModStmt = Param Ident (Maybe Constant)
             | Connect SigSpec SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModStmt where
      pretty = \ case
            Param x c   -> text "parameter" <+> pretty x <+> pretty c
            Connect a b -> text "connect" <+> pretty a <+> pretty b

data Cell = Cell CellType Ident [Stmt CellBody]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Cell where
      pretty (Cell t x body) = vsep $ (text "cell" <+> pretty t <+> pretty x) : map pretty body

data WireOption = WireWidth Natural
                | WireOffset Natural
                | WireInput Natural
                | WireOutput Natural
                | WireInOut Natural
                | WireUpto
                | WireSigned
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty WireOption where
      pretty = \ case
            WireWidth x  -> text "width" <+> pretty x
            WireOffset x -> text "offset" <+> pretty x
            WireInput x  -> text "input" <+> pretty x
            WireOutput x -> text "output" <+> pretty x
            WireInOut x  -> text "inout" <+> pretty x
            WireUpto     -> text "upto"
            WireSigned   -> text "signed"

data CellBody = CellParam (Maybe CellParamSpec) Ident Constant
              | CellConnect Ident SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty CellBody where
      pretty = \ case
            CellParam Nothing x c -> text "parameter" <+> pretty x <+> pretty c
            CellParam cps x c     -> text "parameter" <+> pretty cps <+> pretty x <+> pretty c
            CellConnect x s       -> text "connect" <+> pretty x <+> pretty s

data CellParamSpec = CellSigned
                   | CellReal
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty CellParamSpec where
      pretty = \ case
            CellSigned -> text "signed"
            CellReal   -> text "real"

data SigSpec = SigConstant Constant
             | SigWire Ident
             | SigSelect SigSpec Integer (Maybe Integer)
             | SigConcat [SigSpec]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty SigSpec where
      pretty = \ case
            SigConstant x          -> pretty x
            SigWire x              -> pretty x
            SigSelect s x Nothing  -> pretty s <+> brackets (pretty x)
            SigSelect s x (Just y) -> pretty s <+> brackets (pretty x <> colon <> pretty y)
            SigConcat []           -> text "{}"
            SigConcat ss           -> braces $ hsep $ [mempty] <> map pretty ss <> [mempty]

--- Utility

text :: Text -> Doc a
text = pretty

ppAutoIdx :: Maybe (Stmt AutoIdx) -> [Doc a]
ppAutoIdx = \ case
      Nothing       -> []
      Just (Stmt x) -> [text "autoidx" <+> pretty x]

