import qualified Haskellator

import Data.List (isSuffixOf)
import System.Directory (listDirectory, setCurrentDirectory)
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory, takeFileName)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

import Paths_haskellator (getDataFileName)

getTests :: IO TestTree
getTests = do
      yosys <- testFiles "test/corpus/yosys"
      reg   <- testFiles "test/corpus/regression"
      pure $ testGroup "RTLIL Parser Test Suite"
           [ testGroup "Yosys RTLIL examples" $ map getTest yosys
           , testGroup "Regression" $ map getTest reg
           ]
      where getTest :: FilePath -> TestTree
            getTest c = let gold = c
                            out  = c -<.> "out"
                  in goldenVsFileDiff (takeBaseName c) diff gold out
                   $ setCurrentDirectory (takeDirectory c) >> Haskellator.run [Haskellator.FlagO out, Haskellator.FlagP] [takeFileName c]

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-bBu", ref, new]

            testFiles :: FilePath -> IO [FilePath]
            testFiles path = do
                  dir <- getDataFileName path
                  map (dir </>) . filter (".il" `isSuffixOf`) <$> listDirectory dir

main :: IO ()
main = getTests >>= defaultMain
