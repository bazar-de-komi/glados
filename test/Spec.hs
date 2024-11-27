import Test.Hspec
import StructureSE.StructureSE
import Parser_LISP_SE.Parserlispsexp

main :: IO ()
main = hspec $ do
    describe "Example test" $ do
        it "should pass" $ do
            True `shouldBe` True

    describe "StructureSe" $ do
        it "should show Atom correctly" $ do
            show (Atom "x") `shouldBe` "x"

        it "should show List correctly" $ do
            show (List [Atom "x", Atom "y"]) `shouldBe` "(x y)"

    describe "Parser Lisp SE" $ do
        it "parses a single atom" $ do
            parseSExpr "x" `shouldBe` Just (Atom "x")

        it "parse a simple list" $ do
            parseSExpr "(x y)" `shouldBe` Just (List [Atom "x", Atom "y"])

        it "parses a nested list" $ do
            parseSExpr "(x (y z))" `shouldBe` Just (List [Atom "x", List [Atom "y", Atom "z"]])

        it "returns Nothing for invalid input" $ do
            parseSExpr "(x y" `shouldBe` Nothing