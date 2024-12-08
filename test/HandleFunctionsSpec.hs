module HandleFunctionsSpec (spec) where

import Test.Hspec
import Structure (AST(..))
import HandleAST.HandleFunctions (bindParameters, findBinding, substituteBindings, handleFunctions)

spec :: Spec
spec = do
    describe "HandleFunctions" $ do
        testParameterBind
        testFindBinding
        testSubstituteBindings
        testHandleFunctions

testParameterBind :: Spec
testParameterBind =
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

        describe "No binding possible (Nothing case)" $ do
            it "should return Nothing if the number of parameters and values do not match" $ do
                let params = SList [SSymbol "x", SSymbol "y"]
                let values = SList [SInt 42]
                bindParameters params values `shouldBe` Nothing

            it "should return Nothing if there are too many parameters" $ do
                let params = SList [SSymbol "x", SSymbol "y", SSymbol "z"]
                let values = SList [SInt 42, SInt 84]
                bindParameters params values `shouldBe` Nothing

            it "should return Nothing if parameters are empty but values are non-empty" $ do
                let params = SList []
                let values = SList [SInt 42]
                bindParameters params values `shouldBe` Nothing

            it "should return Nothing if values are empty but parameters are non-empty" $ do
                let params = SList [SSymbol "x"]
                let values = SList []
                bindParameters params values `shouldBe` Nothing

            it "should return Just an empty list if both parameters and values are empty" $ do
                let params = SList []
                let values = SList []
                bindParameters params values `shouldBe` Just (SList [])

            it "should return Nothing if the parameters is not a list" $ do
                let params = SSymbol "x"
                let values = SList [SInt 42]
                bindParameters params values `shouldBe` Nothing

            it "should return Nothing if the values is not a list" $ do
                let params = SList [SSymbol "x"]
                let values = SInt 42
                bindParameters params values `shouldBe` Nothing

testFindBinding :: Spec
testFindBinding = do
    describe "findBinding test" $ do
        it "should return Just value if binding is found" $ do
            let bindings =  SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SInt 84
                                ]
                            ]
            findBinding "x" bindings `shouldBe` Just (SInt 42)

        it "should return Nothing if binding is not found" $ do
            let bindings =  SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SInt 84
                                ]
                            ]
            findBinding "z" bindings `shouldBe` Nothing

        it "should return Nothing if bindings list is empty" $ do
            let bindings = SList []
            findBinding "x" bindings `shouldBe` Nothing

testSubstituteBindings :: Spec
testSubstituteBindings = do
    describe "substituteBindings test" $ do
        it "should substitute the parameter with its value if found" $ do
            let bindings =  SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SInt 84
                                ]
                            ]
            substituteBindings bindings (SSymbol "x") `shouldBe` SInt 42

        it "should return the symbol itself if not found in bindings" $ do
            let bindings =  SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SInt 84
                                ]
                            ]
            substituteBindings bindings (SSymbol "z") `shouldBe` SSymbol "z"

        it "should substitute in a list of symbols" $ do
            let bindings =  SList [
                                SList [
                                    SSymbol "x",
                                    SInt 42
                                ],
                                SList [
                                    SSymbol "y",
                                    SInt 84
                                ]
                            ]
            let ast = SList [SSymbol "x", SSymbol "y", SSymbol "z"]
            let expected = SList [SInt 42, SInt 84, SSymbol "z"]
            substituteBindings bindings ast `shouldBe` expected

testHandleFunctions :: Spec
testHandleFunctions = do
    describe "Handle function tests" $ do
        it "Simple function" $ do
            let ast = SList []
            let function = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "add",
                                SList [
                                    SSymbol "x",
                                    SSymbol "y"
                                ]
                            ],
                            SList [
                                SSymbol "+",
                                SSymbol "x",
                                SSymbol "y"
                            ]
                        ]
            let values = SList [
                            SInt 42,
                            SInt 84
                        ]
            let result = Just (SInt 126)
            handleFunctions ast function values `shouldBe` result

        it "Simple lambda function" $ do
            let ast = SList []
            let function = SList [
                            SSymbol "lambda",
                            SList [
                                SSymbol "x",
                                SSymbol "y"
                            ],
                            SList [
                                SSymbol "+",
                                SSymbol "x",
                                SSymbol "y"
                            ]
                        ]
            let values = SList [
                            SInt 5,
                            SInt 10
                        ]
            let result = Just (SInt 15)
            handleFunctions ast function values `shouldBe` result

        it "Function with lambda inside define" $ do
            let ast = SList []
            let function = SList [
                            SSymbol "define",
                            SSymbol "add",
                            SList [
                                SSymbol "lambda",
                                SList [
                                    SSymbol "x",
                                    SSymbol "y"
                                ],
                                SList [
                                    SSymbol "+",
                                    SSymbol "x",
                                    SSymbol "y"
                                ]
                            ]
                        ]
            let values = SList [
                            SInt 7,
                            SInt 3
                        ]
            let result = Just (SInt 10)
            handleFunctions ast function values `shouldBe` result

    describe "handleFunctions should return Nothing when parameters cannot be bound in lambda function inside of named function" $ do
        it "should return Nothing if the number of parameters and values do not match" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many parameters" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [
                                    SSymbol "x",
                                    SSymbol "y"
                                ],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many values" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42, SInt 84]  -- Two values, but lambda expects one parameter
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are empty but values are non-empty" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are empty but parameters are non-empty" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList []
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are not a list" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SSymbol "x",
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are not a list" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SInt 42
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing for an invalid function definition" $ do
            let ast =   SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

    describe "handleFunctions should return Nothing when parameters cannot be bound in named function" $ do
        it "should return Nothing if the number of parameters and values do not match" $ do
            let ast = SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SSymbol "x"
                            ],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many parameters" $ do
            let ast = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "f",
                                SSymbol "x",
                                SSymbol "y"
                            ],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many values" $ do
            let ast = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "f",
                                SSymbol "x"
                            ],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42, SInt 84]  -- Two values, but lambda expects one parameter
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are empty but values are non-empty" $ do
            let ast = SList [
                            SSymbol "define",
                            SList [SSymbol "f"],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are empty but parameters are non-empty" $ do
            let ast = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "f",
                                SSymbol "x"
                            ],
                            SSymbol "body"
                        ]
            let values = SList []
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are not a list" $ do
            let ast = SList [
                            SSymbol "define",
                            SSymbol "f",
                            SSymbol "x",
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are not a list" $ do
            let ast = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "f",
                                SSymbol "x"
                            ],
                            SSymbol "body"
                        ]
            let values = SInt 42
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing for an invalid function definition" $ do
            let ast = SList [
                            SSymbol "define",
                            SSymbol "f",
                            SList [
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

    describe "handleFunctions should return Nothing when parameters cannot be bound in lambda function" $ do
        it "should return Nothing if the number of parameters and values do not match" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [SSymbol "x"],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many parameters" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [SSymbol "x", SSymbol "y"],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if there are too many values" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [SSymbol "x"],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42, SInt 84]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are empty but values are non-empty" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [],
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are empty but parameters are non-empty" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [SSymbol "x"],
                            SSymbol "body"
                        ]
            let values = SList []
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if parameters are not a list" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SSymbol "x",
                            SSymbol "body"
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing if values are not a list" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [SSymbol "x"],
                            SSymbol "body"
                        ]
            let values = SInt 42
            handleFunctions ast ast values `shouldBe` Nothing

        it "should return Nothing for an invalid lambda function" $ do
            let ast = SList [
                            SSymbol "lambda",
                            SList [
                                SList [SSymbol "x"],
                                SSymbol "body"
                            ]
                        ]
            let values = SList [SInt 42]
            handleFunctions ast ast values `shouldBe` Nothing
