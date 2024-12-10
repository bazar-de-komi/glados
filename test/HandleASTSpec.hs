module HandleASTSpec (spec) where

import Test.Hspec
--import System.IO.Silently (capture)
import Structure (AST(..))
import HandleAST.HandleAST (returnValueAST)
--import HandleAST.HandleAST (handleAST)

spec :: Spec
spec = do
    describe "HandleFunctions" $ do
        testReturnValueAST
--        testHandleAST

testReturnValueAST :: Spec
testReturnValueAST =
    describe "returnValueAST" $ do
        it "Handles integers correctly" $ do
            let ast = SInt 42
            returnValueAST (SList []) ast `shouldBe` Just (SInt 42)

        it "Handles booleans correctly" $ do
            let ast = SBool True
            returnValueAST (SList []) ast `shouldBe` Just (SBool True)

        it "Resolves a symbol" $ do
            let env =   SList [
                            SList [
                                SSymbol "x",
                                SInt 10
                            ]
                        ]
            returnValueAST env (SSymbol "x") `shouldBe` Nothing

        it "Resolves a symbol to its value" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 10
                            ]
                        ]
            returnValueAST env (SSymbol "x") `shouldBe` Just (SInt 10)

        it "Returns the SSymbol if it's not found" $ do
            let env = SList []
            returnValueAST env (SSymbol "y") `shouldBe` Nothing

        it "Handles addition" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol "+", SSymbol "x", SSymbol "y"]
                        ]
            returnValueAST env env `shouldBe` Just (SInt 7)
            returnValueAST (SList []) (SList [SSymbol "+", SSymbol "x", SSymbol "y"]) `shouldBe` Nothing

        it "Handles subtraction" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol "-", SSymbol "x", SSymbol "y"]
                        ]
            returnValueAST env env `shouldBe` Just (SInt $ -1)
            returnValueAST (SList []) (SList [SSymbol "-", SInt 10, SInt 3]) `shouldBe` Just (SInt 7)

        it "Handles multiplication" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol "*", SSymbol "x", SSymbol "y"]
                        ]
            let expr = SList [SSymbol "*", SInt 2, SInt 5]
            returnValueAST env env `shouldBe` Just (SInt 12)
            returnValueAST env expr `shouldBe` Just (SInt 10)

        it "Handles nested operations" $ do
            let env = SList []
            let expr = SList [SSymbol "+", SInt 1, SList [SSymbol "*", SInt 2, SInt 3]]
            returnValueAST env expr `shouldBe` Just (SInt 7)

        it "Handles comparisons" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol ">", SSymbol "x", SSymbol "y"]
                        ]
            let env2 =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol "<", SSymbol "x", SSymbol "y"]
                        ]
            let expr1 = SList [SSymbol "<", SInt 3, SInt 5]
            let expr2 = SList [SSymbol ">", SInt 5, SInt 3]
            returnValueAST env env `shouldBe` Just (SBool False)
            returnValueAST env2 env2 `shouldBe` Just (SBool True)
            returnValueAST env expr1 `shouldBe` Just (SBool True)
            returnValueAST env expr2 `shouldBe` Just (SBool True)

        it "Handles equality checks" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 3
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 4
                            ],
                            SList [SSymbol "eq?", SSymbol "x", SSymbol "y"]
                        ]
            let expr = SList [SSymbol "eq?", SInt 42, SInt 42]
            returnValueAST env env `shouldBe` Just (SBool False)
            returnValueAST env expr `shouldBe` Just (SBool True)

        it "Handles division operation" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 9
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 3
                            ],
                            SList [SSymbol "div", SSymbol "x", SSymbol "y"]
                        ]
            let expr = SList [SSymbol "div", SInt 9, SInt 3]
            returnValueAST env env `shouldBe` Just (SInt 3)
            returnValueAST env expr `shouldBe` Just (SInt 3)

        it "Handles modulo operation" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 9
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 3
                            ],
                            SList [SSymbol "mod", SSymbol "x", SSymbol "y"]
                        ]
            let expr = SList [SSymbol "mod", SInt 10, SInt 3]
            returnValueAST env env `shouldBe` Just (SInt 0)
            returnValueAST env expr `shouldBe` Just (SInt 1)

        it "Returns Nothing for unsupported operations" $ do
            let env = SList []
            let expr = SList [SSymbol "unsupported", SInt 1, SInt 2]
            returnValueAST env expr `shouldBe` Nothing

        it "Handles a lambda function correctly" $ do
            let env = SList []
            let expr = SList [
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SList [SSymbol "+", SSymbol "x", SInt 1]
                            ],
                            SInt 5
                        ]
            returnValueAST env expr `shouldBe` Just (SInt 6)

        it "Handles nested lambda calls" $ do
            let env = SList []
            let expr = SList [
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SList [
                                    SList [
                                        SSymbol "lambda",
                                        SList [SSymbol "y"],
                                        SList [SSymbol "+", SSymbol "x", SSymbol "y"]
                                    ],
                                    SInt 3
                                ]
                            ],
                            SInt 4
                        ]
            returnValueAST env expr `shouldBe` Just (SInt 7)

        it "Handles undefined symbols within a lambda body" $ do
            let env = SList []
            let expr = SList [
                            SList [
                                SSymbol "lambda",
                                SList [SSymbol "x"],
                                SList [SSymbol "+", SSymbol "x", SSymbol "y"]
                            ],
                            SInt 2
                        ]
            returnValueAST env expr `shouldBe` Nothing

        it "Handles symbols resolved via getWithDefine" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "a",
                                SInt 5
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "b",
                                SInt 5
                            ],
                            SList [
                                SSymbol "define",
                                SList [
                                    SSymbol "add",
                                    SSymbol "x",
                                    SSymbol "y"
                                ],
                                SList [
                                    SSymbol "+",
                                    SSymbol "a",
                                    SSymbol "b"
                                ]
                            ],
                            SList [
                                SSymbol "add",
                                SInt 5,
                                SInt 5
                            ]
                        ]
            returnValueAST env env `shouldBe` Just (SInt 10)

        it "Handles symbols resolved via getWithDefine with lambda" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "add",
                                SList [
                                    SSymbol "lambda",
                                    SList [
                                        SSymbol "a",
                                        SSymbol "b"
                                    ],
                                    SList [
                                        SSymbol "+",
                                        SSymbol "a",
                                        SSymbol "b"
                                    ]
                                ]
                            ],
                            SList [
                                SSymbol "add",
                                SInt 5,
                                SInt 5
                            ]
                        ]
            returnValueAST env env `shouldBe` Just (SInt 10)
        
        it "Bad formatted lambda" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "add",
                                SList [
                                    SSymbol "lambda",
                                    SSymbol "a",
                                    SSymbol "b",
                                    SList [
                                        SSymbol "+",
                                        SSymbol "a",
                                        SSymbol "b"
                                    ]
                                ]
                            ],
                            SList [
                                SSymbol "add",
                                SInt 5,
                                SInt 5
                            ]
                        ]
            returnValueAST env env `shouldBe` Nothing

        it "Handles deeply nested define statements" $ do
            let env =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SList [
                                    SList [
                                        SSymbol "define",
                                        SSymbol "y",
                                        SInt 10
                                    ],
                                    SSymbol "y"
                                ]
                            ],
                            SSymbol "x"
                        ]
            let result =    SList [
                                SList [
                                    SSymbol "define",
                                    SSymbol "y",
                                    SInt 10
                                ],
                                SSymbol "y"
                            ]
            returnValueAST env env `shouldBe` Just result

        it "Returns Nothing for a malformed list" $ do
            let env = SList []
            let expr = SList [SSymbol "+", SInt 1]
            returnValueAST env expr `shouldBe` Nothing

        it "Handles invalid nested operations" $ do
            let env = SList []
            let expr = SList [SSymbol "+", SInt 1, SList [SSymbol "+", SInt 2]]
            returnValueAST env expr `shouldBe` Nothing

        it "Handles empty lists correctly" $ do
            let env = SList []
            let expr = SList []
            returnValueAST env expr `shouldBe` Nothing

        it "Handles a single item list" $ do
            let env = SList []
            let expr = SList [SInt 42]
            returnValueAST env expr `shouldBe` Just (SInt 42)

        it "Handles a non-symbol in function position" $ do
            let env = SList []
            let expr = SList [SInt 3, SInt 4]
            returnValueAST env expr `shouldBe` Nothing

        it "Handles SList with SList correctly" $ do
            let innerAST = SInt 42
            let ast = SList [SList [innerAST]]
            returnValueAST (SList []) ast `shouldBe` Just (SInt 42)

        it "Handles an empty list as input" $ do
            let ast = SList []
            returnValueAST (SList []) ast `shouldBe` Nothing

--testHandleAST :: Spec
--testHandleAST =
--    describe "handleAST" $ do
--        it "Prints the evaluated AST if evaluation succeeds" $ do
--            let ast = SInt 42
--            (output, _) <- capture $ handleAST (Just ast)
--            output `shouldBe` "42\n"
--
--        it "Prints an error message if input is Nothing" $ do
--            (output, _) <- capture $ handleAST Nothing
--            output `shouldBe` "ERROR: Failed to parse check your Lisp expression!\n"
--
--        it "Prints an error if no return value is obtained" $ do
--            let ast = SList []
--            (output, _) <- capture $ handleAST (Just ast)
--            output `shouldBe` "ERROR: Failed no return value!\n"
--
--        it "Prints the correct evaluation result for valid input" $ do
--            let ast = SList [SSymbol "+", SInt 1, SInt 2]
--            (output, _)<- capture $ handleAST (Just ast)
--            output `shouldBe` "3\n"
