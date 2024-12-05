module OperatorsSpec (spec) where

import Test.Hspec
import Structure (AST(..))
import HandleAST.Operators (eq, lt, add, subtractAST, multiply, divAST, modAST)

spec :: Spec
spec = do
    describe "Operators" $ do
        testEquality
        testLessThan
        testAddition
        testSubtraction
        testMultiplication
        testDivision
        testModulo

testEquality :: Spec
testEquality = describe "eq (Equality)" $ do
    it "returns Just (SBool True) for equal integers" $ do
        eq (Just (SInt 42)) (Just (SInt 42)) `shouldBe` Just (SBool True)

    it "returns Just (SBool False) for non-equal integers" $ do
        eq (Just (SInt 42)) (Just (SInt 10)) `shouldBe` Just (SBool False)

    it "returns Just (SBool True) for equal symbols" $ do
        eq (Just (SSymbol "x")) (Just (SSymbol "x")) `shouldBe` Just (SBool True)

    it "returns Just (SBool False) for non-equal symbols" $ do
        eq (Just (SSymbol "x")) (Just (SSymbol "y")) `shouldBe` Just (SBool False)

    it "returns Nothing when the type is not compatible" $ do
        eq (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testLessThan :: Spec
testLessThan = describe "lt (Less-Than)" $ do
    it "returns Just (SBool True) if the first integer is less than the second" $ do
        lt (Just (SInt 1)) (Just (SInt 42)) `shouldBe` Just (SBool True)

    it "returns Just (SBool False) if the first integer is not less than the second" $ do
        lt (Just (SInt 42)) (Just (SInt 10)) `shouldBe` Just (SBool False)

    it "returns Nothing when the type is not compatible" $ do
        lt (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testAddition :: Spec
testAddition = describe "add (Addition)" $ do
    it "returns the sum of two integers" $ do
        add (Just (SInt 10)) (Just (SInt 32)) `shouldBe` Just (SInt 42)

    it "concatenates two symbols" $ do
        add (Just (SSymbol "Hello")) (Just (SSymbol "World")) `shouldBe` Just (SSymbol "HelloWorld")

    it "returns Nothing when the type is not compatible" $ do
        add (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testSubtraction :: Spec
testSubtraction = describe "subtractAST (Subtraction)" $ do
    it "returns the difference of two integers" $ do
        subtractAST (Just (SInt 42)) (Just (SInt 10)) `shouldBe` Just (SInt 32)

    it "returns Nothing when the type is not compatible" $ do
        subtractAST (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testMultiplication :: Spec
testMultiplication = describe "multiply (Multiplication)" $ do
    it "returns the product of two integers" $ do
        multiply (Just (SInt 6)) (Just (SInt 7)) `shouldBe` Just (SInt 42)

    it "returns Nothing when the type is not compatible" $ do
        multiply (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testDivision :: Spec
testDivision = describe "divAST (Division)" $ do
    it "returns the quotient of two integers" $ do
        divAST (Just (SInt 42)) (Just (SInt 2)) `shouldBe` Just (SInt 21)

    it "returns Nothing for division by zero" $ do
        divAST (Just (SInt 42)) (Just (SInt 0)) `shouldBe` Nothing

    it "returns Nothing when the type is not compatible" $ do
        divAST (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing

testModulo :: Spec
testModulo = describe "modAST (Modulo)" $ do
    it "returns the remainder of two integers" $ do
        modAST (Just (SInt 42)) (Just (SInt 5)) `shouldBe` Just (SInt 2)

    it "returns Nothing for modulo by zero" $ do
        modAST (Just (SInt 42)) (Just (SInt 0)) `shouldBe` Nothing

    it "returns Nothing when the type is not compatible" $ do
        modAST (Just (SInt 42)) (Just (SSymbol "x")) `shouldBe` Nothing
