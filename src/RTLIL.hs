{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module RTLIL
    ( main, run, module RTLIL.Flags
    ) where

import RTLIL.Flags (Flag (..))

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr (Option), ArgOrder (Permute), ArgDescr (NoArg, ReqArg))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))
import System.IO (stderr)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

mainOptions :: [OptDescr Flag]
mainOptions =
      [ Option ['v'] ["verbose"]          (NoArg  FlagV)              "More verbose output."
      , Option ['h'] ["help"]             (NoArg  FlagH)              "Print this message."
      , Option ['o'] []                   (ReqArg FlagO "file.out")   "Output file name."
      ]

pInfo :: MonadIO m => Text -> m ()
pInfo = liftIO . T.putStrLn

pErr :: MonadIO m => Text -> m ()
pErr = liftIO . T.hPutStrLn stderr

pFatal :: MonadIO m => Text -> m a
pFatal m = pErr m >> liftIO exitFailure

exitUsage :: MonadIO m => [Text] -> m a
exitUsage msgs = do
      mapM_ pErr msgs
      pFatal (T.pack $ usageInfo "Usage: rtlil [OPTION...] <filename>" mainOptions)

run :: MonadIO m => [Flag] -> [FilePath] -> m ()
run flags args = forM_ args $ \ f -> do
      fout <- getOutFile f
      when verbose $ do
            pInfo $ "Writing to file: " <> T.pack fout
      liftIO $ T.writeFile fout ""

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

main :: IO ()
main = do
      (flags, args, errs) <- getOpt Permute mainOptions <$> getArgs

      when (FlagH `elem` flags || not (null errs) || null args)
            $ exitUsage $ T.pack <$> errs

      run flags args

