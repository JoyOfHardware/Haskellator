{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module RTLIL.Parse (parseFile) where

import qualified RTLIL.Syntax as IL

import Prelude hiding (mod, maybe)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P

parseFile :: MonadIO m => FilePath -> m (Either Text IL.File)
parseFile f = liftIO (handleResult <$> (P.parse (rtlilFile) <$> T.readFile f))
      where handleResult :: P.Result r -> Either Text r
            handleResult = \ case
                  P.Fail _ ctx err -> Left $ T.pack $ intercalate "\n" ("Context:" : (ctx <> ["\nError: ", err]))
                  P.Partial cont   -> handleResult $ cont T.empty
                  P.Done _ r       -> pure r

rtlilFile :: Parser IL.File
rtlilFile = IL.File <$> comments <*> maybe (stmt autoIdx) <*> blocks mod <*> comments
      <?> "RTLIL file"

autoIdx :: Parser IL.AutoIdx
autoIdx = tok "autoidx" *> decimal

mod :: Parser IL.Module
mod = tok "module" *> (IL.Module <$> (ident <* eol) <*> many modBody)
      <?> "module"

attr :: Parser IL.Attr
attr = tok "attribute" *> (IL.Attr <$> ident <*> constant)
      <?> "attribute"

modBody :: Parser IL.ModBody
modBody = IL.ModAttrStmt <$> attrStmt (wire <|> memory)
      <|> IL.ModStmt <$> stmt (param <|> connect)
      <|> IL.ModBlock <$> block (cell <|> proc)
      <?> "module body"

wire :: Parser IL.ModAttrStmt
wire = tok "wire" *> (IL.Wire <$> many wireOption <*> ident)
      <?> "wire"

memory :: Parser IL.ModAttrStmt
memory = tok "memory" *> (IL.Memory <$> many memoryOption <*> ident)
      <?> "memory"

param :: Parser IL.ModStmt
param = tok "parameter" *> (IL.Param <$> ident <*> maybe constant)
      <?> "parameter"

connect :: Parser IL.ModStmt
connect = tok "connect" *> (IL.Connect <$> sigSpec <*> sigSpec)
      <?> "connect"

cell :: Parser IL.ModBlock
cell = tok "cell" *> (IL.Cell <$> ident <*> (ident <* eol) <*> stmts cellBody)
      <?> "cell"

proc :: Parser IL.ModBlock
proc = tok "process" *> (IL.Proc <$> (ident <* eol) <*> stmts assign <*> maybe (block switch) <*> stmts assign <*> stmts' sync)
      <?> "process"

assign :: Parser IL.Assign
assign = tok "assign" *> (IL.Assign <$> sigSpec <*> sigSpec)
      <?> "assign"

switch :: Parser IL.Switch
switch = tok "switch" *> (IL.Switch <$> (sigSpec <* eol) <*> attrStmts kase)
      <?> "switch"

sync :: Parser IL.Sync
sync = tok "sync" *>
      (   tok "global" *> eol *> (IL.SyncGlobal <$> stmts update)
      <|> tok "init"   *> eol *> (IL.SyncInit <$> stmts update)
      <|> tok "always" *> eol *> (IL.SyncAlways <$> stmts update)
      <|>                  IL.SyncSignal <$> syncType <*> sigSpec <*> stmts update
      )

syncType :: Parser IL.SyncType
syncType = tok "low" $> IL.Low
       <|> tok "posedge" $> IL.PosEdge
       <|> tok "negedge" $> IL.NegEdge
       <|> tok "edge" $> IL.Edge

update :: Parser IL.Update
update = tok "update" *> (IL.Update <$> sigSpec <*> sigSpec)
      <?> "update"

kase :: Parser IL.Case
kase = tok "case" *> (IL.Case <$> (sigSpec `P.sepBy` tok ",") <*> many kaseBody)
      <?> "case"

kaseBody :: Parser IL.CaseBody
kaseBody = IL.CaseSwitch <$> block switch
       <|> IL.CaseAssign <$> stmt assign

cellParamSpec :: Parser IL.CellParamSpec
cellParamSpec = tok "signed" $> IL.CellSigned
            <|> tok "real"   $> IL.CellReal
            <?> "cell parameter spec"

cellBody :: Parser IL.CellBody
cellBody = tok "parameter" *> (IL.CellParam   <$> maybe cellParamSpec <*> ident <*> constant)
       <|> tok "connect"   *> (IL.CellConnect <$> ident <*> sigSpec)
       <?> "cell statement"

wireOption :: Parser IL.WireOption
wireOption = tok "width"  *> (IL.WireWidth  <$> decimal)
         <|> tok "offset" *> (IL.WireOffset <$> decimal)
         <|> tok "input"  *> (IL.WireInput  <$> decimal)
         <|> tok "output" *> (IL.WireOutput <$> decimal)
         <|> tok "inout"  *> (IL.WireInOut  <$> decimal)
         <|> tok "upto"   $> IL.WireUpto
         <|> tok "signed" $> IL.WireSigned
         <?> "wire option"

memoryOption :: Parser IL.MemoryOption
memoryOption = tok "width"  *> (IL.MemoryWidth  <$> decimal)
           <|> tok "offset" *> (IL.MemoryOffset <$> decimal)
           <|> tok "size"   *> (IL.MemorySize   <$> decimal)
           <?> "memory option"

sigSpec :: Parser IL.SigSpec
sigSpec = foldl (\ s (n, m) -> IL.SigSelect s n m) <$> sigSpec' <*> many select

select :: Parser (Integer, Maybe Integer)
select = brackets ((,) <$> integer <*> maybe (tok ":" *> integer))
      <?> "signal spec"

sigSpec' :: Parser IL.SigSpec
sigSpec' = (IL.SigConstant <$> constant)
      <|> (IL.SigWire <$> ident)
      <|> sigConcat

sigConcat :: Parser IL.SigSpec
sigConcat = tok "{" *> (IL.SigConcat <$> many sigSpec) <* tok "}"
        <?> "signal spec: concat"

stmts :: Parser a -> Parser [IL.Stmt a]
stmts = many . stmt

-- | No eol.
stmts' :: Parser a -> Parser [IL.Stmt a]
stmts' = many . stmt'

stmt :: Parser a -> Parser (IL.Stmt a)
stmt p = IL.Stmt <$> comments <*> p <* eol

-- | No eol.
stmt' :: Parser a -> Parser (IL.Stmt a)
stmt' p = IL.Stmt <$> comments <*> p

attrStmts :: Parser a -> Parser [IL.AttrStmt a]
attrStmts = many . attrStmt

attrStmt :: Parser a -> Parser (IL.AttrStmt a)
attrStmt p = IL.AttrStmt <$> stmts attr <*> comments <*> p <* eol

blocks :: Parser a -> Parser [IL.Block a]
blocks = many . block

block :: Parser a -> Parser (IL.Block a)
block p = IL.Block <$> stmts attr <*> comments <*> p <*> comments <* tok "end" <* eol

comments :: Parser [IL.Comment]
comments = many comment

comment :: Parser IL.Comment
comment = P.char '#' *> skipSpace *> P.takeTill P.isEndOfLine <* P.endOfLine <* skipSpace

ident :: Parser IL.Ident
ident = public <|> autogen
        <?> "identifier"

public :: Parser IL.Ident
public = P.char '\\' *> (IL.Public <$> P.takeWhile1 (not . isSpace)) <* skipSpace

autogen :: Parser IL.Ident
autogen = P.char '$' *> (IL.Autogen <$> P.takeWhile1 (not . isSpace)) <* skipSpace

constant :: Parser IL.Constant
constant = constantString
      <|> constantValue
      <|> IL.ConstantInteger <$> integer
      <?> "constant"

constantString :: Parser IL.Constant
constantString = P.char '"' *> (IL.ConstantString <$> stringContents) <* P.char '"' <* skipSpace
        <?> "constant: string"

stringContents :: Parser Text
stringContents = (<>) <$> (mconcat <$> many escaped) <*> P.takeWhile (/='"')

escaped :: Parser Text
escaped = (<>) <$> P.takeWhile (\ c -> c /= '\\' && c /= '"') <*> escapeCode

escapeCode :: Parser Text -- TODO: more?
escapeCode = P.string "\\\""
         <|> P.string "\\\\"
         <|> P.string "\\0"
         <|> P.string "\\n"
         <|> P.string "\\t"

constantValue :: Parser IL.Constant
constantValue = IL.ConstantValue <$> decimal <*> (tok "'" *> binary)
        <?> "constant: value"

tok :: Text -> Parser ()
tok s = P.string s *> skipSpace

maybe :: Parser a -> Parser (Maybe a)
maybe p = P.option Nothing (Just <$> p)

decimal :: Integral a => Parser a
decimal = P.decimal <* skipSpace

binary :: Parser Text
binary = P.takeWhile isBinaryDigit <* skipSpace

isBinaryDigit :: Char -> Bool
isBinaryDigit = \ case
      '0' -> True
      '1' -> True
      'x' -> True
      'z' -> True
      'm' -> True
      '-' -> True
      _   -> False

integer :: Parser Integer
integer = P.signed decimal

skipSpace :: Parser ()
skipSpace = P.skipWhile P.isHorizontalSpace

skipAllSpace :: Parser ()
skipAllSpace = P.skipSpace

eol :: Parser ()
eol = P.endOfLine *> skipAllSpace

many :: Parser a -> Parser [a]
many = P.many'

brackets :: Parser a -> Parser a
brackets p = tok "[" *> p <* tok "]"

