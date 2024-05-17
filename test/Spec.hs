import qualified Haskellator

import Data.List (isSuffixOf)
import System.Directory (listDirectory, setCurrentDirectory)
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory, takeFileName)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

import Paths_haskellator (getDataFileName)

getTests :: IO TestTree
getTests = do
      dir  <- getDataFileName "test/corpus"
      rtlils   <- map (dir </>) . filter (".il" `isSuffixOf`)   <$> listDirectory dir
      pure $ testGroup "RTLIL Corpus" $ map getTest rtlils
      where getTest :: FilePath -> TestTree
            getTest c = let gold = c
                            out  = c -<.> "out"
                  in goldenVsFileDiff (takeBaseName c) diff gold out
                   $ setCurrentDirectory (takeDirectory c) >> Haskellator.run [Haskellator.FlagO out, Haskellator.FlagP] [takeFileName c]

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-Bu", ref, new]

main :: IO ()
main = getTests >>= defaultMain
