module ParserCompilVMSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, eof)
import Data.Either (isLeft)
import Structure (Instruction(..), Value(..), BinaryOperator(..), BinaryComparator(..))
import Parser.ParserCompilVM (noSpaceInst, noUselessNInst, nextLineInst, parsIntInst, parsFloatInst, parseStringInst, parseCharInst, parseBoolInst, parseVAlue, parseStoreConst, parseStoreVar, parseLoadVar, parseJump, parseJumpIfFalse, parseLabel, parseLabelFunc, parseLabelFuncEnd, parseCall, parseReturn, parseHalt, parseOperator, parseComparator, parseInstruction, pGroupedExprInst, pProgramInst)

spec :: Spec
spec = do
    describe "Parser Compiler to VM tests" $ do
      testNoSpaceInst
      testNoUselessNInst
      testNextLineInst
      testParsIntInst
      testParsFloatInst
      testParseStringInst
      testParseCharInst
      testParseBoolInst
      testParseVAlue
      testParseStoreConst
      testParseStoreVar
      testParseLoadVar
      testParseJump
      testParseJumpIfFalse
      testParseLabel
      testParseLabelFunc
      testParseLabelFuncEnd
      testParseCall
      testParseReturn
      testParseHalt
      testParseOperator
      testParseComparator
      testParseInstruction
      testParseInstructionOperator
      testParseInstructionComparator
      testPGroupedExprInst
      testPProgramInst

testNoSpaceInst :: Spec
testNoSpaceInst = describe "noSpaceInst" $ do
  it "consumes spaces and tabs" $
    parse (noSpaceInst <* eof) "" "    \t  " `shouldBe` Right ()

  it "consumes a block comment delimited by <-- and -->" $
    parse (noSpaceInst <* eof) "" "<-- This is a block comment -->" `shouldBe` Right ()

  it "fails for single-line comments starting with <-" $
    parse (noSpaceInst <* eof) "" "<- This is a comment\n" `shouldSatisfy` isLeft

  it "handles combinations of spaces, tabs, and comments" $
    let input = "   \t  <- Comment\n<-- Block comment -->   "
    in parse (noSpaceInst <* eof) "" input `shouldSatisfy` isLeft

  it "fails if non-space content exists outside comments" $
    let input = "abc <- Comment"
    in parse (noSpaceInst <* eof) "" input `shouldSatisfy` isLeft

testNoUselessNInst :: Spec
testNoUselessNInst = describe "noUselessNInst" $ do
  it "skips a single newline" $
    parse (noUselessNInst <* eof) "" "\n" `shouldBe` Right ()

  it "does not skip multiple newlines" $
    parse (noUselessNInst <* eof) "" "\n\n" `shouldSatisfy` isLeft

testNextLineInst :: Spec
testNextLineInst = describe "nextLineInst" $ do
  it "consumes trailing spaces and single newlines" $
    parse (nextLineInst <* eof) "" "   \n  " `shouldBe` Right ()

  it "fails if multiple newlines are present" $
    parse (nextLineInst <* eof) "" "\n\n" `shouldSatisfy` isLeft

testParsIntInst :: Spec
testParsIntInst = describe "parsIntInst" $ do
  it "parses a positive integer without sign" $
    parse parsIntInst "" "42" `shouldBe` Right 42

  it "parses a positive integer with '+' sign" $
    parse parsIntInst "" "+42" `shouldBe` Right 42

  it "parses a negative integer with '-' sign" $
    parse parsIntInst "" "-42" `shouldBe` Right (-42)

  it "fails for letters" $
    parse parsIntInst "" "abc" `shouldSatisfy` isLeft

testParsFloatInst :: Spec
testParsFloatInst = describe "parsFloatInst" $ do
  it "parses a positive float without sign" $
    parse parsFloatInst "" "42.5" `shouldBe` Right 42.5

  it "parses a positive float with '+' sign" $
    parse parsFloatInst "" "+42.5" `shouldBe` Right 42.5

  it "parses a negative float with '-' sign" $
    parse parsFloatInst "" "-42.5" `shouldBe` Right (-42.5)

  it "fails for integer input" $
    parse parsFloatInst "" "42" `shouldSatisfy` isLeft

  it "fails if a float is followed by letters" $
    parse parsFloatInst "" "42.5abc" `shouldSatisfy` isLeft

  it "fails for non-float input" $
    parse parsFloatInst "" "abc" `shouldSatisfy` isLeft

testParseStringInst :: Spec
testParseStringInst = describe "parseStringInst" $ do
  it "parses a quoted string" $
    parse parseStringInst "" "\"hello\"" `shouldBe` Right "hello"

  it "fails for an unquoted string" $
    parse parseStringInst "" "hello" `shouldSatisfy` isLeft

testParseCharInst :: Spec
testParseCharInst = describe "parseCharInst" $ do
  it "parses a single quoted character" $
    parse parseCharInst "" "'a'" `shouldBe` Right 'a'

  it "fails for multiple characters" $
    parse parseCharInst "" "'abc'" `shouldSatisfy` isLeft

testParseBoolInst :: Spec
testParseBoolInst = describe "parseBoolInst" $ do
  it "parses True" $
    parse parseBoolInst "" "True\n" `shouldBe` Right True

  it "parses False" $
    parse parseBoolInst "" "False\n" `shouldBe` Right False

  it "fails for invalid boolean input" $
    parse parseBoolInst "" "NotBool" `shouldSatisfy` isLeft

testParseVAlue :: Spec
testParseVAlue = describe "parseVAlue" $ do
  it "parses an integer value" $
    parse parseVAlue "" "42" `shouldBe` Right (VInt 42)

  it "parses a float value" $
    parse parseVAlue "" "3.14" `shouldBe` Right (VFloat 3.14)

  it "parses a string value" $
    parse parseVAlue "" "\"hello\"" `shouldBe` Right (VString "hello")

  it "parses a boolean value" $
    parse parseVAlue "" "True\n" `shouldBe` Right (VBool True)

  it "parses a character value" $
    parse parseVAlue "" "'c'" `shouldBe` Right (VChar 'c')

testParseStoreConst :: Spec
testParseStoreConst = describe "parseStoreConst" $ do
  it "parses STORE_CONST with an integer" $
    parse parseStoreConst "" "STORE_CONST 42" `shouldBe` Right (STORE_CONST (VInt 42))

  it "parses STORE_CONST with a string" $
    parse parseStoreConst "" "STORE_CONST \"hello\"" `shouldBe` Right (STORE_CONST (VString "hello"))

testParseStoreVar :: Spec
testParseStoreVar = describe "parseStoreVar" $ do
  it "parses STORE_VAR with a variable name" $
    parse parseStoreVar "" "STORE_VAR \"myVar\"" `shouldBe` Right (STORE_VAR "myVar")

  it "fails for invalid STORE_VAR syntax" $
    parse parseStoreVar "" "STORE_VAR myVar" `shouldSatisfy` isLeft

-- Tests pour parseLoadVar
testParseLoadVar :: Spec
testParseLoadVar = describe "parseLoadVar" $ do
  it "parses LOAD_VAR with a variable name" $
    parse parseLoadVar "" "LOAD_VAR \"myVar\"" `shouldBe` Right (LOAD_VAR "myVar")

  it "fails for invalid LOAD_VAR syntax" $
    parse parseLoadVar "" "LOAD_VAR myVar" `shouldSatisfy` isLeft

testParseJump :: Spec
testParseJump = describe "parseJump" $ do
  it "parses JUMP with a target label" $
    parse parseJump "" "JUMP \"target\"" `shouldBe` Right (JUMP "target")

  it "fails for invalid JUMP syntax" $
    parse parseJump "" "JUMP target" `shouldSatisfy` isLeft

testParseJumpIfFalse :: Spec
testParseJumpIfFalse = describe "parseJumpIfFalse" $ do
  it "parses JUMP_IF_FALSE with a target label" $
    parse parseJumpIfFalse "" "JUMP_IF_FALSE \"target\"" `shouldBe` Right (JUMP_IF_FALSE "target")

  it "fails for invalid JUMP_IF_FALSE syntax" $
    parse parseJumpIfFalse "" "JUMP_IF_FALSE target" `shouldSatisfy` isLeft

testParseLabel :: Spec
testParseLabel = describe "parseLabel" $ do
  it "parses LABEL with a label name" $
    parse parseLabel "" "LABEL \"label\"" `shouldBe` Right (LABEL "label")

  it "fails for invalid LABEL syntax" $
    parse parseLabel "" "LABEL label" `shouldSatisfy` isLeft

testParseLabelFunc :: Spec
testParseLabelFunc = describe "parseLabelFunc" $ do
  it "parses LABEL_FUNC with a label name" $
    parse parseLabelFunc "" "LABEL_FUNC \"funcLabel\"" `shouldBe` Right (LABEL_FUNC "funcLabel")

  it "fails for invalid LABEL_FUNC syntax" $
    parse parseLabelFunc "" "LABEL_FUNC funcLabel" `shouldSatisfy` isLeft

testParseLabelFuncEnd :: Spec
testParseLabelFuncEnd = describe "parseLabelFuncEnd" $ do
  it "parses LABEL_FUNC_END with a label name" $
    parse parseLabelFuncEnd "" "LABEL_FUNC_END \"funcEnd\"" `shouldBe` Right (LABEL_FUNC_END "funcEnd")

  it "fails for invalid LABEL_FUNC_END syntax" $
    parse parseLabelFuncEnd "" "LABEL_FUNC_END funcEnd" `shouldSatisfy` isLeft

testParseCall :: Spec
testParseCall = describe "parseCall" $ do
  it "parses CALL with a function label" $
    parse parseCall "" "CALL \"myFunc\"" `shouldBe` Right (CALL "myFunc")

  it "fails for invalid CALL syntax" $
    parse parseCall "" "CALL myFunc" `shouldSatisfy` isLeft

testParseReturn :: Spec
testParseReturn = describe "parseReturn" $ do
  it "parses RETURN" $
    parse parseReturn "" "RETURN" `shouldBe` Right RETURN

  it "fails for invalid RETURN syntax" $
    parse parseReturn "" "RETURN extra" `shouldBe` Right RETURN

testParseHalt :: Spec
testParseHalt = describe "parseHalt" $ do
  it "parses HALT" $
    parse parseHalt "" "HALT" `shouldBe` Right HALT

  it "fails for invalid HALT syntax" $
    parse parseHalt "" "HALT extra" `shouldBe` Right HALT

testParseOperator :: Spec
testParseOperator = describe "parseOperator" $ do
  it "parses ADD operator" $
    parse parseOperator "" "OPERATOR ADD" `shouldBe` Right ADD

  it "parses SUBTRACT operator" $
    parse parseOperator "" "OPERATOR SUBTRACT" `shouldBe` Right SUBTRACT

  it "parses MULTIPLY operator" $
    parse parseOperator "" "OPERATOR MULTIPLY" `shouldBe` Right MULTIPLY

  it "parses DIVIDE operator" $
    parse parseOperator "" "OPERATOR DIVIDE" `shouldBe` Right DIVIDE

  it "parses MODULO operator" $
    parse parseOperator "" "OPERATOR MODULO" `shouldBe` Right MODULO

  it "fails for invalid operator syntax" $
    parse parseOperator "" "OPERATOR UNKNOWN" `shouldSatisfy` isLeft

testParseComparator :: Spec
testParseComparator = describe "parseComparator" $ do
  it "parses COMPARE_GT comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_GT" `shouldBe` Right COMPARE_GT

  it "parses COMPARE_LT comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_LT" `shouldBe` Right COMPARE_LT

  it "parses COMPARE_EQ comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_EQ" `shouldBe` Right COMPARE_EQ

  it "parses COMPARE_NE comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_NE" `shouldBe` Right COMPARE_NE

  it "parses COMPARE_GE comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_GE" `shouldBe` Right COMPARE_GE

  it "parses COMPARE_LE comparator" $
    parse parseComparator "" "COMPARATOR COMPARE_LE" `shouldBe` Right COMPARE_LE

  it "fails for invalid comparator syntax" $
    parse parseComparator "" "COMPARATOR UNKNOWN" `shouldSatisfy` isLeft

testParseInstruction :: Spec
testParseInstruction = describe "parseInstruction" $ do
  it "parses a STORE_CONST instruction" $
    parse parseInstruction "" "STORE_CONST 42" `shouldBe` Right (STORE_CONST (VInt 42))

  it "parses a CALL instruction" $
    parse parseInstruction "" "CALL \"myFunc\"" `shouldBe` Right (CALL "myFunc")

  it "fails for invalid instruction syntax" $
    parse parseInstruction "" "UNKNOWN 123" `shouldSatisfy` isLeft

testParseInstructionOperator :: Spec
testParseInstructionOperator = describe "parseInstruction with OPERATOR" $ do
  it "parses an ADD operator" $
    parse parseInstruction "" "OPERATOR ADD" `shouldBe` Right (OPERATOR ADD)

  it "parses a SUBTRACT operator" $
    parse parseInstruction "" "OPERATOR SUBTRACT" `shouldBe` Right (OPERATOR SUBTRACT)

  it "parses a MULTIPLY operator" $
    parse parseInstruction "" "OPERATOR MULTIPLY" `shouldBe` Right (OPERATOR MULTIPLY)

  it "parses a DIVIDE operator" $
    parse parseInstruction "" "OPERATOR DIVIDE" `shouldBe` Right (OPERATOR DIVIDE)

  it "parses a MODULO operator" $
    parse parseInstruction "" "OPERATOR MODULO" `shouldBe` Right (OPERATOR MODULO)

  it "fails for an invalid operator" $
    parse parseInstruction "" "OPERATOR INVALID_OP" `shouldSatisfy` isLeft

testParseInstructionComparator :: Spec
testParseInstructionComparator = describe "parseInstruction with COMPARATOR" $ do
  it "parses a COMPARE_GT comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_GT" `shouldBe` Right (COMPARATOR COMPARE_GT)

  it "parses a COMPARE_LT comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_LT" `shouldBe` Right (COMPARATOR COMPARE_LT)

  it "parses a COMPARE_EQ comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_EQ" `shouldBe` Right (COMPARATOR COMPARE_EQ)

  it "parses a COMPARE_NE comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_NE" `shouldBe` Right (COMPARATOR COMPARE_NE)

  it "parses a COMPARE_GE comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_GE" `shouldBe` Right (COMPARATOR COMPARE_GE)

  it "parses a COMPARE_LE comparator" $
    parse parseInstruction "" "COMPARATOR COMPARE_LE" `shouldBe` Right (COMPARATOR COMPARE_LE)

  it "fails for an invalid comparator" $
    parse parseInstruction "" "COMPARATOR INVALID_CMP" `shouldSatisfy` isLeft

testPGroupedExprInst :: Spec
testPGroupedExprInst = describe "pGroupedExprInst" $ do
  it "parses a group of instructions" $
    parse pGroupedExprInst "" "STORE_CONST 42\nCALL \"myFunc\"" `shouldBe`
      Right [STORE_CONST (VInt 42), CALL "myFunc"]

  it "fails for invalid group of instructions" $
    parse pGroupedExprInst "" "STORE_CONST 42\nUNKNOWN 123\n" `shouldSatisfy` isLeft

testPProgramInst :: Spec
testPProgramInst = describe "pProgramInst" $ do
  it "parses a complete program" $
    parse pProgramInst "" "STORE_CONST 42\nCALL \"myFunc\"\nHALT" `shouldBe`
      Right [STORE_CONST (VInt 42), CALL "myFunc", HALT]

  it "fails for invalid program" $
    parse pProgramInst "" "STORE_CONST 42\nINVALID 123\n" `shouldSatisfy` isLeft
