{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Haskellator
    ( main, run, module Haskellator.Flags
    ) where

import Haskellator.Flags (Flag (..))
import RTLIL.Parse (parseFile)

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (Option), ArgOrder (Permute), ArgDescr (NoArg, ReqArg))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import System.IO (stderr)
import Text.Show.Pretty (ppShow)

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Prettyprinter             as P
import qualified Prettyprinter.Render.Text as P

mainOptions :: [OptDescr Flag]
mainOptions =
      [ Option ['v'] ["verbose"]          (NoArg  FlagV)              "More verbose output."
      , Option ['h'] ["help"]             (NoArg  FlagH)              "Print this message."
      , Option ['p'] ["pretty"]           (NoArg  FlagP)              "Write pretty-printed output instead of AST (for testing)."
      , Option ['o'] []                   (ReqArg FlagO "file.out")   "Output file name."
      ]

layoutOptions :: P.LayoutOptions
layoutOptions = P.defaultLayoutOptions { P.layoutPageWidth = P.AvailablePerLine 120 1.0 }

prettyPrint :: P.Pretty a => a -> Text
prettyPrint = P.renderStrict . P.layoutSmart layoutOptions . P.pretty

pInfo :: MonadIO m => Text -> m ()
pInfo = liftIO . T.putStrLn

pErr :: MonadIO m => Text -> m ()
pErr = liftIO . T.hPutStrLn stderr

pFatal :: MonadIO m => Text -> m a
pFatal m = pErr m >> liftIO exitFailure

exitUsage :: MonadIO m => [Text] -> m a
exitUsage msgs = do
      mapM_ pErr msgs
      pFatal (T.pack $ usageInfo "Usage: rtlil-parse [OPTION...] <filename>" mainOptions)

run :: MonadIO m => [Flag] -> [FilePath] -> m ()
run flags args = forM_ args $ \ f -> do
      when verbose $ pInfo $ "Parsing file: " <> T.pack f

      result <- parseFile f >>= either pFatal pure
      fout <- getOutFile f

      when verbose $ do
            pInfo $ "AST: "
            pInfo $ T.pack $ ppShow result

      when verbose $ do
            pInfo $ "Pretty: "
            pInfo $ prettyPrint result

      when verbose $ pInfo $ "Writing to file: " <> T.pack fout
      liftIO $ T.writeFile fout $ if writePretty then prettyPrint result else T.pack $ ppShow result

      where getOutFile :: MonadIO m => String -> m String
            getOutFile filename = case filter flagO flags of
                  [FlagO o] -> pure o
                  []        -> pure $ filename -<.> "out"
                  _         -> do
                        pErr "Multiple output files specified on the command line!"
                        liftIO exitFailure

            flagO :: Flag -> Bool
            flagO = \ case
                  FlagO {} -> True
                  _        -> False

            verbose :: Bool
            verbose = FlagV `elem` flags

            writePretty :: Bool
            writePretty = FlagP `elem` flags

main :: IO ()
main = do
      (flags, args, errs) <- getOpt Permute mainOptions <$> getArgs

      when (FlagH `elem` flags || not (null errs) || null args)
            $ exitUsage $ T.pack <$> errs

      run flags args

