{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module RTLIL.Syntax
      ( AutoIdx, UpTo, Signed, Offset, PortIdx, Size, CellType, Comment
      , Ident (..), AttrStmt (..), Stmt (..), Block (..), Attr (..)
      , File (..), Module (..)
      , ModBody (..), ModAttrStmt (..), ModStmt (..), ModBlock (..)
      , WireOption (..), MemoryOption (..)
      , Assign (..), Switch (..), Case (..), CaseBody (..)
      , Sync (..), SyncType (..), Update (..)
      , CellBody (..), CellParamSpec (..)
      , Constant (..), SigSpec (..)
      ) where

import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter (Pretty (..), hsep, vsep, nest, (<+>), Doc, colon, braces, brackets, punctuate, dquotes, squote)

type AutoIdx = Integer
type UpTo = Bool
type Signed = Bool
type Offset = Natural
type PortIdx = Int
type Size = Natural
type CellType = Ident
type Comment = Text

data Ident = Public Text
           | Autogen Text
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Ident where
      pretty = \ case
            Public x  -> text $ "\\" <> x
            Autogen x -> text $ "$" <> x

data AttrStmt a = AttrStmt [Stmt Attr] [Comment] a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (AttrStmt a) where
      pretty (AttrStmt as cs x) = vsep $ map pretty as <> ppComs cs <> [pretty x]

data Stmt a = Stmt [Comment] a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (Stmt a) where
      pretty (Stmt cs x) = vsep $ ppComs cs <> [pretty x]

data Block a = Block [Stmt Attr] [Comment] a
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty a => Pretty (Block a) where
      pretty (Block as cs x) = vsep $ map pretty as <> ppComs cs <> [nest 2 $ pretty x, text "end"]

data Attr = Attr Ident Constant
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Attr where
      pretty (Attr x c) = text "attribute" <+> pretty x <+> pretty c

data File = File [Comment] (Maybe (Stmt AutoIdx)) [Block Module]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty File where
      pretty (File cs idx body) = vsep $ ppComs cs <> ppAutoIdx idx <> map pretty body <> [mempty]

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
             | ModBlock (Block ModBlock)
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModBody where
      pretty = \ case
            ModAttrStmt s -> pretty s
            ModStmt s     -> pretty s
            ModBlock s    -> pretty s

data ModAttrStmt = Wire [WireOption] Ident
                 | Memory [MemoryOption] Ident
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModAttrStmt where
      pretty = \ case
            Wire [] x    -> text "wire" <+> pretty x
            Wire ops x   -> text "wire" <+> hsep (map pretty ops) <+> pretty x
            Memory [] x  -> text "memory" <+> pretty x
            Memory ops x -> text "memory" <+> hsep (map pretty ops) <+> pretty x

data ModStmt = Param Ident (Maybe Constant)
             | Connect SigSpec SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModStmt where
      pretty = \ case
            Param x c   -> text "parameter" <+> pretty x <+> pretty c
            Connect a b -> text "connect" <+> pretty a <+> pretty b

data ModBlock = Cell CellType Ident [Stmt CellBody]
              | Proc Ident [Stmt Assign] (Maybe (Block Switch)) [Stmt Assign] [Stmt Sync]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty ModBlock where
      pretty = \ case
            Cell t x body         -> vsep $ (text "cell" <+> pretty t <+> pretty x) : map pretty body
            Proc x ss sw ss' ss'' -> vsep $ [text "process" <+> pretty x] <> map pretty ss <> [pretty sw] <> map pretty ss' <> map pretty ss''

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

data MemoryOption = MemoryWidth Natural
                  | MemorySize Natural
                  | MemoryOffset Natural
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty MemoryOption where
      pretty = \ case
            MemoryWidth x  -> text "width" <+> pretty x
            MemorySize x   -> text "size" <+> pretty x
            MemoryOffset x -> text "offset" <+> pretty x

data Assign = Assign SigSpec SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Assign where
      pretty (Assign a b) = text "assign" <+> pretty a <+> pretty b

data Switch = Switch SigSpec [AttrStmt Case]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Switch where
      pretty (Switch s cs) = vsep $ (text "switch" <+> pretty s) : map pretty cs

data Case = Case [SigSpec] [CaseBody]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Case where
      pretty = \ case
            Case [] body -> vsep $ text "case" : map pretty body
            Case ss body -> vsep $ (text "case" <+> (hsep $ punctuate (text ", ") $ pretty <$> ss)) : map pretty body

data CaseBody = CaseSwitch (Block Switch)
              | CaseAssign (Stmt Assign)
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty CaseBody where
      pretty = \ case
            CaseSwitch a -> pretty a
            CaseAssign a -> pretty a

data Sync = SyncSignal SyncType SigSpec [Stmt Update]
          | SyncGlobal [Stmt Update]
          | SyncInit [Stmt Update]
          | SyncAlways [Stmt Update]
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Sync where
      pretty = \ case
            SyncSignal t s ups -> vsep $ (text "sync" <+> pretty t <+> pretty s) : map pretty ups
            SyncGlobal ups     -> vsep $ text "sync global" : map pretty ups
            SyncInit ups       -> vsep $ text "sync init" : map pretty ups
            SyncAlways ups     -> vsep $ text "sync always" : map pretty ups

data SyncType = Low
              | PosEdge
              | NegEdge
              | Edge
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty SyncType where
      pretty = \ case
            Low     -> text "low"
            PosEdge -> text "posedge"
            NegEdge -> text "negedge"
            Edge    -> text "edge"

data Update = Update SigSpec SigSpec
      deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance Pretty Update where
      pretty (Update a b) = text "update" <+> pretty a <+> pretty b

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
            SigConcat ss           -> braces $ hsep $ [mempty] <> map pretty ss <> [mempty]

--- Utility

text :: Text -> Doc a
text = pretty

ppComs :: [Comment] -> [Doc a]
ppComs = map ppCom
      where ppCom :: Comment -> Doc a
            ppCom c = text "#" <+> text c

ppAutoIdx :: Maybe (Stmt AutoIdx) -> [Doc a]
ppAutoIdx = \ case
      Nothing          -> []
      Just (Stmt cs x) -> ppComs cs <> [text "autoidx" <+> pretty x]


