module Main where

import System.Environment (getArgs)
import System.IO
import Control.Exception (catch, IOException)
import Text.Show.Pretty (ppShow)

import Haskellator

main :: IO ()
main = do
    -- Get the command-line arguments
    args <- getArgs

    -- Check if a file name is provided
    case args of
        (filePath:_) -> do
            -- Attempt to read the file
            contents <- catch (readFile filePath) handleReadError
            putStrLn "File Contents:"
            putStrLn $ Haskellator.preProcessDiscardComments contents
        [] -> putStrLn "cabal run Haskellator -- <file-path>"
    putStrLn $ ppShow Haskellator.val

-- Handle potential file reading errors
handleReadError :: IOException -> IO String
handleReadError _ = return "Error: Could not read the file."
