module ParserSExpASTSpec (spec) where

import Test.Hspec
import Structure (SExpr(..), AST(..))
import Parser.ParserSExpAST (lastCheck, parseFinalAST, findFuncOrDef, findIf, findLoop, findFor, findCall, checkFinalAST)

spec :: Spec
spec = do
  describe "Function and Definition Parsing (findFuncOrDef)" $ do
    it "parses a function with parameters and a body" $ do
      let input = [Atom "add", Type "int", Param [Atom "a", Atom "b"], List [BasicFunc "+", Atom "a", Atom "b"]]
      findFuncOrDef input `shouldBe` Right (SFunc "add" (SType "int") (SList [SVariable "a", SVariable "b"]) (SList [SOperation "+", SVariable "a", SVariable "b"]))

    it "parses a definition with a body" $ do
      let input = [Atom "x", Type "int", List [SEInt 42]]
      findFuncOrDef input `shouldBe` Right (SDefine "x" (SType "int") (SList [SInt 42]))

    it "returns an error for unsupported input" $ do
      let input = [Atom "x"]
      findFuncOrDef input `shouldBe` Left "Unsupported function or definition: "

    it "returns an error for unsupported input no param" $ do
      let input = [Atom "add", List [BasicFunc "+", Atom "a", Atom "b"]]
      findFuncOrDef input `shouldBe` Left "Unsupported function or definition: "


  describe "Conditional Parsing (findIf)" $ do
    it "parses a conditional if expression" $ do
      let input = SEIf (Param [Atom "x", BasicFunc ">", Atom "y"]) (List [Atom "result", Atom "x"]) (List [Atom "result", Atom "y"])
      findIf input `shouldBe` Right (SIf (SList [SVariable "x", SOperation ">", SVariable "y"]) (SList [SVariable "result", SVariable "x"]) (SList [SVariable "result", SVariable "y"]))

    it "returns an error for unsupported input" $ do
      let input = SEInt 42
      findIf input `shouldBe` Left "Unsupported SExpr: SEInt 42"

  describe "Loop Parsing (findLoop)" $ do
    it "parses a while loop" $ do
      let input = SELoop (Param [Atom "i", BasicFunc "<", SEInt 10]) (List [BasicFunc "+=", Atom "i", SEInt 1])
      findLoop input `shouldBe` Right (SLoop (SList [SVariable "i", SOperation "<", SInt 10]) (SList [SOperation "+=", SVariable "i", SInt 1]))

    it "returns an error for unsupported input" $ do
      let input = SEInt 42
      findLoop input `shouldBe` Left "Unsupported SExpr: SEInt 42"

  describe "For Loop Parsing (findFor)" $ do
    it "parses a for loop" $ do
      let input = SEFor (List [BasicFunc "=", Atom "i", SEInt 0]) (Param [Atom "i", BasicFunc "<", SEInt 10]) (List [BasicFunc "+=", Atom "i", SEInt 1]) (List [Atom "print", Atom "i"])
      findFor input `shouldBe` Right (SFor (SList [SOperation "=", SVariable "i", SInt 0]) (SList [SVariable "i", SOperation "<", SInt 10]) (SList [SOperation "+=", SVariable "i", SInt 1]) (SList [SVariable "print", SVariable "i"]))

    it "returns an error for unsupported input" $ do
      let input = SEInt 42
      findFor input `shouldBe` Left "Unsupported SExpr: SEInt 42"

  describe "Function Call Parsing (findCall)" $ do
    it "parses a function call" $ do
      let input = List [Atom "name", List [SEInt 1, SEInt 2]]
      findCall input `shouldBe` Right (SCall "name" (SList [SInt 1, SInt 2]))

    it "returns an error for unsupported input" $ do
      let input = SEInt 42
      findCall input `shouldBe` Left "Unsupported SExpr: SEInt 42"

  describe "AST Conversion (parseFinalAST)" $ do
    it "converts basic SExprs to AST" $ do
      parseFinalAST (SEInt 42) `shouldBe` Right (SInt 42)
      parseFinalAST (SEFloat 3.14) `shouldBe` Right (SFloat 3.14)
      parseFinalAST (Boolean True) `shouldBe` Right (SBool True)

    it "parses a return expression" $ do
      let input = Return (List [SEInt 42])
      parseFinalAST input `shouldBe` Right (SReturn (SList [SInt 42]))

    it "parses a function definition" $ do
      let input = List [Atom "add", BasicFunc ":", Type "int", Param [Atom "a", Atom "b"], List [BasicFunc "+", Atom "a", Atom "b"]]
      parseFinalAST input `shouldBe` Right (SFunc "add" (SType "int") (SList [SVariable "a", SVariable "b"]) (SList [SOperation "+", SVariable "a", SVariable "b"]))

    it "parses nested SLists" $ do
      let input = List [SEInt 1, SEList [SEInt 2, SEInt 3]]
      parseFinalAST input `shouldBe` Right (SList [SInt 1, SListOf [SInt 2, SInt 3]])

  describe "AST Validation (checkFinalAST)" $ do
    it "detects duplicate functions or definitions" $ do
      let ast = [SFunc "test" (SType "int") (SList []) (SList []), SFunc "test" (SType "int") (SList []) (SList [])]
      checkFinalAST ast `shouldBe` "ERROR test is initialized 2 times"

    it "detects adjacent variables without an operation" $ do
      let ast = [SVariable "a", SVariable "b"]
      checkFinalAST ast `shouldBe` "ERROR : a b with no operation"

    it "returns an empty string for valid ASTs" $ do
      let ast = [SFunc "test1" (SType "int") (SList []) (SList []), SFunc "test2" (SType "int") (SList []) (SList [])]
      checkFinalAST ast `shouldBe` ""

  describe "Final AST Check (lastCheck)" $ do
    it "validates a correct AST" $ do
      let ast = SList [SFunc "test" (SType "int") (SList []) (SList [])]
      lastCheck (Right ast) `shouldBe` Right ast

    it "returns an error for duplicate names" $ do
      let ast = SList [SFunc "test" (SType "int") (SList []) (SList []), SFunc "test" (SType "int") (SList []) (SList [])]
      lastCheck (Right ast) `shouldBe` Left "ERROR test is initialized 2 times"

    it "returns an error for unexpected AST nodes" $ do
      lastCheck (Right (SInt 42)) `shouldBe` Left "ERROR Unexpected AST :SInt 42"
