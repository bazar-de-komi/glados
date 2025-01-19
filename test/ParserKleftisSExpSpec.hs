module ParserKleftisSExpSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import Parser.ParserKleftisSExp (pProgram, parseString, parsInt, parsFloat, parseBool, parseList, parseIf, parseLoop, parseFor)
import Structure (SExpr(..))

spec :: Spec
spec = do
  describe "Basic parsers" $ do
    it "parses integers correctly" $ do
      parse parsInt "" "42" `shouldBe` Right 42
      parse parsInt "" "-123" `shouldBe` Right (-123)

    it "parses integers not correctly" $ do
      parse parsInt "" "abc" `shouldSatisfy` either (const True) (const False)

    it "parses floating-point numbers correctly" $ do
      parse parsFloat "" "3.14" `shouldBe` Right 3.14
      parse parsFloat "" "-0.5" `shouldBe` Right (-0.5)

    it "parses floating-point numbers not correctly" $ do
      parse parsFloat "" "abc" `shouldSatisfy` either (const True) (const False)

    it "parses strings correctly" $ do
      parse parseString "" "#@hello world!#" `shouldBe` Right "hello world!"

    it "parses strings not correctly" $ do
      parse parseString "" "#@unterminated" `shouldSatisfy` either (const True) (const False)
      parse parseString "" "#p" `shouldSatisfy` either (const True) (const False)
      parse parseString "" "coucou" `shouldSatisfy` either (const True) (const False)

    it "parses booleans correctly" $ do
      parse parseBool "" "True " `shouldBe` Right True
      parse parseBool "" "False " `shouldBe` Right False

    it "parses booleans not correctly" $ do
      parse parseBool "" "Maybe " `shouldSatisfy` either (const True) (const False)

  describe "Structured parsers" $ do
    it "parses lists correctly" $ do
      parse parseList "" "(a b c)" `shouldBe` Right (List [Atom "a", Atom "b", Atom "c"])

    it "parses lists not correctly" $ do
      parse parseList "" "(a b c" `shouldSatisfy` either (const True) (const False)

    it "parses nested lists correctly" $ do
      let input = "(a (b c) d)"
      parse parseList "" input `shouldBe` Right (List [Atom "a", List [Atom "b", Atom "c"], Atom "d"])

    it "parses nested lists not correctly" $ do
      parse parseList "" "(a (b c d" `shouldSatisfy` either (const True) (const False)

    it "parses if expressions correctly" $ do
      let input = "si [a > b] (result a) sinon (result b)"
      parse parseIf "" input `shouldBe` Right (SEIf (Param [Atom "a", BasicFunc ">", Atom "b"]) 
                                                   (List [Return (List [Atom "a"])]) 
                                                   (List [Return (List [Atom "b"])]))

    it "parses if expressions correctly without sinon" $ do
      parse parseIf "" "si [a > b] (result a)" `shouldBe` Right (SEIf (Param [Atom "a", BasicFunc ">", Atom "b"]) 
                                                                     (List [Return (List [Atom "a"])]) 
                                                                     (List []))

    it "parses while loop expressions correctly" $ do
      let input = "tankeu [x < 10] (result x)"
      parse parseLoop "" input `shouldBe` Right (SELoop (Param [Atom "x", BasicFunc "<", SEInt 10]) 
                                                        (List [Return (List [Atom "x"])]))

    it "parses while loop expressions not correctly" $ do
      parse parseLoop "" "tankeu [x < 10]" `shouldSatisfy` either (const True) (const False)

    it "parses for loop expressions correctly" $ do
      let input = "pour (= i 0) [i < 10] (i += 1) (result i)"
      parse parseFor "" input `shouldBe` Right (SEFor (List [BasicFunc "=", Atom "i", SEInt 0])
                                                      (Param [Atom "i", BasicFunc "<", SEInt 10])
                                                      (List [Atom "i", BasicFunc "+=", SEInt 1])
                                                      (List [Return (List [Atom "i"])]))

    it "parses for loop expressions correctly missing update clause" $ do
      parse parseFor "" "pour (= i 0) [i < 10] (result i)" `shouldSatisfy` either (const True) (const False)

  describe "Program parser" $ do
    it "parses a complete program with multiple expressions" $ do
      let input = 
            "(define x 42)\n\n" ++
            "(define y 3.14)\n\n" ++
            "si [x > y] (result x) sinon (result y)"
      parse pProgram "" input `shouldBe` Right (List [List [List [Atom "define", Atom "x", SEInt 42]], List [List [Atom "define", Atom "y", SEFloat 3.14]], List [SEIf (Param [Atom "x", BasicFunc ">", Atom "y"]) (List [Return (List [Atom "x"])]) (List [Return (List [Atom "y"])])]])

    it "handles syntax errors gracefully" $ do
      let input = "(define x 42"
      case parse pProgram "" input of
        Left err -> errorBundlePretty err `shouldContain` "unexpected end of input"
        Right _  -> expectationFailure "Parsing should have failed"
