module IntegrationSpec (spec) where

import Test.Hspec
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

runGLaDOS :: String -> IO (ExitCode, String, String)
runGLaDOS input = readCreateProcessWithExitCode (shell $ "echo '" ++ input ++ "' | ./glados") ""

spec :: Spec
spec = do
    describe "Example tests" $ do
      it "should pass" $ do
        1 `shouldBe` 1
