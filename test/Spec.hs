import qualified RTLIL

import Data.List (isSuffixOf)
import System.Directory (listDirectory, setCurrentDirectory)
import System.FilePath ((</>), (-<.>), takeBaseName, takeDirectory, takeFileName)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

import Paths_haskellator (getDataFileName)

getTests :: IO TestTree
getTests = do
      dir  <- getDataFileName "test/corpus"
      golds <- map (dir </>) . filter (".gold" `isSuffixOf`) <$> listDirectory dir
      rtlils   <- map (dir </>) . filter (".il" `isSuffixOf`)   <$> listDirectory dir
      pure $ testGroup "RTLIL Corpus" $ map getTest $ filter ((`elem` golds) . (-<.> "gold")) rtlils
      where getTest :: FilePath -> TestTree
            getTest c = let gold = c -<.> "gold"
                            out  = c -<.> "out"
                  in goldenVsFileDiff (takeBaseName c) diff gold out
                   $ setCurrentDirectory (takeDirectory c) >> RTLIL.run [RTLIL.FlagO out] [takeFileName c]

            diff :: FilePath -> FilePath -> [String]
            diff ref new = ["diff", "-bu", ref, new]

main :: IO ()
main = getTests >>= defaultMain
