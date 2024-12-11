module Main where

import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import Text.Show.Pretty (ppShow)

import Haskellator

main :: IO ()
main = do
    -- Get the command-line arguments
    args <- getArgs

    -- Check if the input and output file names are provided
    case args of
        (inputFilePath:outputFilePath:_) -> do
            contents <- catch (readFile inputFilePath) handleReadError
            let output = ppShow $ Haskellator.runParser contents inputFilePath
            catch (writeFile outputFilePath output) handleWriteError
            putStrLn $ "Output written to " ++ outputFilePath
        _ -> putStrLn "Usage: cabal run rtlil-parse -- <input-file-path> <output-file-path>"

-- Handle potential file reading errors
handleReadError :: IOException -> IO String
handleReadError _ = return "Error: Could not read the input file."

-- Handle potential file writing errors
handleWriteError :: IOException -> IO ()
handleWriteError _ = putStrLn "Error: Could not write to the output file."