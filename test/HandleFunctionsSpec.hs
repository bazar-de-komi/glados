module HandleFunctionsSpec (spec) where

import Test.Hspec
import Structure (AST(..))
import HandleAST.HandleFunctions (bindParameters)

spec :: Spec
spec = do
    describe "Parameter binding test" $ do
        it "Bind one parameter" $ do
            let param = SList [
                            SSymbol "x"
                        ]
            let value = SList [
                            SInt 42
                        ]
            let result = SList [
                            SList [
                                SSymbol "x",
                                SInt 42
                            ]
                        ]
            bindParameters param value `shouldBe` Just result

        it "Bind two parameter" $ do
            let param = SList [
                            SSymbol "x",
                            SSymbol "y"
                        ]
            let value = SList [
                            SInt 42,
                            SBool True
                        ]
            let result = SList [
                            SList [
                                SSymbol "x",
                                SInt 42
                            ],
                            SList [
                                SSymbol "y",
                                SBool True
                            ]
                        ]
            bindParameters param value `shouldBe` Just result

        it "Bind three parameter" $ do
                let param = SList [
                                SSymbol "x",
                                SSymbol "y",
                                SSymbol "hello"
                            ]
                let value = SList [
                                SInt 42,
                                SBool True,
                                SSymbol "hola"
                            ]
                let result = SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SBool True
                                ],
                                SList [
                                    SSymbol "hello",
                                    SSymbol "hola"
                                ]
                            ]
                bindParameters param value `shouldBe` Just result

        it "returns Nothing if the number of parameters and values don't match" $ do
            let param1 = SList [
                            SSymbol "x",
                            SSymbol "y"
                        ]
            let value1 = SList [SInt 42]
            let param2 = SList [SSymbol "x"]
            let value2 = SList [SInt 42, SInt 84]
            bindParameters param1 value1 `shouldBe` Nothing
            bindParameters param2 value2 `shouldBe` Nothing
