import Test.Hspec
import StructureSE.StructureSE
import Parser_LISP_SE.Parserlispsexp

main :: IO ()
main = hspec $ do
    describe "Example test" $ do
        it "should pass" $ do
            True `shouldBe` True
