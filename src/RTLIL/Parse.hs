module RTLIL.Parse (parseFile) where

import qualified RTLIL.Syntax as IL

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.Text (Parser, parse, IResult (Fail, Partial, Done))
import Data.Functor ((<&>))

import qualified Data.Text.IO as T

parseFile :: MonadIO m => FilePath -> m (Either ([String], String) IL.File)
parseFile f = liftIO $ (parse rtlilFile <$> T.readFile f) <&> (\ case
      Fail _ ctxt msg -> Left (ctxt, msg)
      Partial _       -> Left ([], "Partial parse")
      Done _ r        -> Right r)

rtlilFile :: Parser IL.File
rtlilFile = pure $ IL.File Nothing [] -- TODO
