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
    describe "Functional Tests" $ do
      
      it "should pass" $ do
        show "test" `shouldBe` "\"test\""