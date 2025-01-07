module IntegrationSpec (spec) where

import Test.Hspec
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import qualified Data.Text as T

executable :: FilePath
executable = "./glados"

runTest :: FilePath -> IO (ExitCode, String, String)
runTest inputFile = do
    input <- readFile inputFile
    readCreateProcessWithExitCode (shell $ "echo '" ++ input ++ "' | " ++ executable) ""

spec :: Spec
spec = do 
    describe "Functional Tests : Basic fonctionality, check normal behavior" $ do
      
      it "handles simple condition expressions (if1.scm)" $ do
        let inputFile = "functionalTest/inputs/if1.scm"
        let expectedFile = "functionalTest/expected/if1.out"

        (_, output, _) <- runTest inputFile
        expectedOutput <- readFile expectedFile

        T.unpack (T.strip (T.pack output)) `shouldBe` T.unpack (T.strip (T.pack expectedOutput))

      it "evaluates an addition expression (addition.scm)" $ do
        let addTestFile = "functionalTest/inputs/addition.scm"
        let expectedTestFile = "functionalTest/expected/addition.out"
  
        (_, output, _) <- runTest addTestFile
        expectedTestFile <- readFile expectedTestFile

        T.unpack (T.strip (T.pack output)) `shouldBe` T.unpack (T.strip (T.pack expectedTestFile))