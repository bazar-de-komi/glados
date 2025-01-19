module ParserKleftisSExpSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import Parser.ParserKleftisSExp (pProgram)
import Structure (SExpr(..))

spec :: Spec
spec = do
  describe "Functional tests for the parser" $ do
    it "parses a full program with multiple definitions and expressions" $ do
      let input = unlines
            ["si [x > y] (result x) sinon (result y)"
            , "tantque [x < 10] (result x)"
            , "pour (= i 0) [i < 10] (i += 1) (result i)"
            ]
      let expected = List
            [ List [SEIf (Param [Atom "x", BasicFunc ">", Atom "y"])
                   (List [Return (List [Atom "x"])])
                   (List [Return (List [Atom "y"])])
            , SELoop (Param [Atom "x", BasicFunc "<", SEInt 10])
                     (List [Return (List [Atom "x"])])
            , SEFor (List [BasicFunc "=", Atom "i", SEInt 0])
                    (Param [Atom "i", BasicFunc "<", SEInt 10])
                    (List [Atom "i", BasicFunc "+=", SEInt 1])
                    (List [Return (List [Atom "i"])])]
            ]
      parse pProgram "" input `shouldBe` Right expected

    it "parses nested expressions correctly" $ do
      let input = unlines
            [ "define z (si [x > y] (result x) sinon (result y))"
            , "define list {1 2 3}"
            , "tantque [z < 10] (result z)"
            ]
      let expected = Right (List [List [Atom "define", Atom "z", List [SEIf (Param [Atom "x", BasicFunc ">", Atom "y"]) (List [Return (List [Atom "x"])]) (List [Return (List [Atom "y"])])], Atom "define", Atom "list", SEList [SEInt 1, SEInt 2, SEInt 3], SELoop (Param [Atom "z", BasicFunc "<", SEInt 10]) (List [Return (List [Atom "z"])])]])
      parse pProgram "" input `shouldBe` expected

    it "parses a complete program with conditionals, loops, and lists" $ do
      let input = unlines
            [ "define max (si [a > b] (result a) sinon (result b))"
            , "pour (= i 0) [i < 10] (i += 1) (result i)"
            , "tantque [i < 100] (result i)"
            ]
      let expected = List
            [ List [Atom "define", Atom "max"
                   , List[ SEIf (Param [Atom "a", BasicFunc ">", Atom "b"])
                          (List [Return (List [Atom "a"])])
                          (List [Return (List [Atom "b"])])
                   ]
            , SEFor (List [BasicFunc "=", Atom "i", SEInt 0])
                    (Param [Atom "i", BasicFunc "<", SEInt 10])
                    (List [Atom "i", BasicFunc "+=", SEInt 1])
                    (List [Return (List [Atom "i"])])
            , SELoop (Param [Atom "i", BasicFunc "<", SEInt 100])
                     (List [Return (List [Atom "i"])])]
            ]
      parse pProgram "" input `shouldBe` Right expected

    it "handles invalid syntax gracefully" $ do
      let input = "define x 42\n\nsi [x > y] (result x" -- Missing closing parenthesis
      case parse pProgram "" input of
        Left err -> errorBundlePretty err `shouldContain` "unexpected end of input"
        Right _  -> expectationFailure "Parsing should have failed"
