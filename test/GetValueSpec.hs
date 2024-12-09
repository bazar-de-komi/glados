module GetValueSpec (spec) where

import Test.Hspec
import HandleAST.GetValue (getValue, getWithDefine)
import Structure (AST (..))

spec :: Spec
spec = do
    describe "AST getter" $ do
        testGetValue
        testGetWithDefine

testGetValue :: Spec
testGetValue =
    describe "getValue Basic Test" $ do
    it "Search when the AST is a SInt" $ do
        getValue (SInt 42) (SSymbol "x") `shouldBe` Nothing
        getValue (SInt 0) (SSymbol "x") `shouldBe` Nothing

    it "Search when the AST is a SSymbol" $ do
        getValue (SSymbol "hello") (SSymbol "x") `shouldBe` Nothing
        getValue (SSymbol "party") (SSymbol "x") `shouldBe` Nothing

    it "Search when the AST is a SBool" $ do
        getValue (SBool True) (SSymbol "x") `shouldBe` Nothing
        getValue (SBool False) (SSymbol "x") `shouldBe` Nothing

    it "Search when the list is empty" $ do
        getValue (SList []) (SSymbol "x") `shouldBe` Nothing

    it "Search when the list does not contain what we want" $ do
        let input = SList [
                        SInt 20,
                        SBool True,
                        SSymbol "hello"
                    ]
        getValue input (SSymbol "x") `shouldBe` Nothing
        getValue input (SSymbol "hello") `shouldBe` Nothing

    it "Search in a simple list" $ do
        let input = SList [
                        SSymbol "define",
                        SSymbol "x",
                        SInt 42
                    ]
        getValue input (SSymbol "x") `shouldBe` Just (SInt 42)
        getValue input (SSymbol "xa") `shouldBe` Nothing
        getValue input (SSymbol "ax") `shouldBe` Nothing

    it "Search when the value is in a sublist" $ do
        let input = SList [
                        SBool True,
                        SSymbol "party",
                        SList [
                            SSymbol "define",
                            SSymbol "x",
                            SInt 42
                        ],
                        SInt 84
                    ]
        getValue input (SSymbol "x") `shouldBe` Just (SInt 42)
        getValue input (SSymbol "xa") `shouldBe` Nothing
        getValue input (SSymbol "ax") `shouldBe` Nothing
        getValue input (SSymbol "") `shouldBe` Nothing
        getValue input (SSymbol "party") `shouldBe` Nothing
        getValue input (SSymbol "hello") `shouldBe` Nothing

    describe "getValue Advanced Test" $ do
        it "returns the first matching value in a list of multiple bindings" $ do
            let input = SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 42
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 99
                            ]
                        ]
            getValue input (SSymbol "x") `shouldBe` Just (SInt 42)
            getValue input (SSymbol "xa") `shouldBe` Nothing
            getValue input (SSymbol "ax") `shouldBe` Nothing

        it "Search when some list does not contain a define" $ do
            let input = SList [
                            SList [
                                SSymbol "define",
                                SSymbol "x",
                                SInt 42
                            ],
                            SList [
                                SSymbol "+",
                                SInt 1,
                                SInt 2
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "y",
                                SInt 99
                            ]
                        ]
            getValue input (SSymbol "x") `shouldBe` Just (SInt 42)
            getValue input (SSymbol "y") `shouldBe` Just (SInt 99)
            getValue input (SSymbol "+") `shouldBe` Nothing
            getValue input (SSymbol "ya") `shouldBe` Nothing
            getValue input (SSymbol "ay") `shouldBe` Nothing

        it "Search the value in a complex structure" $ do
            let input = SList [
                            SList [
                                SSymbol "+",
                                SInt 1,
                                SInt 2
                            ],
                            SList [
                                SSymbol "define",
                                SList [
                                    SSymbol "<",
                                    SSymbol "a",
                                    SSymbol "b"
                                ],
                                SList [
                                    SSymbol "*",
                                    SInt 8,
                                    SInt 20
                                ],
                                SSymbol "ok",
                                SList [
                                    SSymbol "define",
                                        SList [
                                        SSymbol "eq?",
                                        SSymbol "x",
                                        SSymbol "y"
                                    ],
                                    SList [
                                        SList [
                                            SSymbol "+",
                                            SInt 10,
                                            SInt 15
                                        ],
                                        SSymbol "hello"
                                    ]
                                ]
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "azerty",
                                SSymbol "yolo"
                            ]
                        ]
            getValue input (SList [SSymbol "<", SSymbol "a", SSymbol "b"]) `shouldBe` Just (SList [SSymbol "*", SInt 8, SInt 20])
            getValue input (SList [SSymbol "eq?", SSymbol "x", SSymbol "y"]) `shouldBe` Just (
                SList [
                    SList [
                        SSymbol "+",
                        SInt 10,
                        SInt 15
                    ],
                    SSymbol "hello"
                ])
            getValue input (SSymbol "azerty") `shouldBe` Just (SSymbol "yolo")
            getValue input (SSymbol "+") `shouldBe` Nothing
            getValue input (SSymbol "*") `shouldBe` Nothing
            getValue input (SSymbol "hello") `shouldBe` Nothing
            getValue input (SSymbol "yolo") `shouldBe` Nothing
            getValue input (SSymbol "party") `shouldBe` Nothing

testGetWithDefine :: Spec
testGetWithDefine =
    describe "getWithDefine" $ do
        it "returns the function definition if the target matches a top-level define" $ do
            let ast =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "x"],
                                    SSymbol "body"
                                ]
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "g",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "y"],
                                    SSymbol "body2"
                                ]
                            ]
                        ]
            getWithDefine ast (SSymbol "f") `shouldBe` Just (SList [SSymbol "define", SSymbol "f", SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "body"]])
            -- getWithDefine ast (SSymbol "f") `shouldBe` Nothing

        it "should return the full definition of the 'add' function" $ do
            let ast =   SList [
                            SList [
                                SSymbol "define",
                                SList [
                                    SSymbol "add",
                                    SSymbol "a",
                                    SSymbol "b"
                                ],
                                SList [
                                    SSymbol "+",
                                    SSymbol "a",
                                    SSymbol "b"
                                ]
                            ]
                        ]
            let target = SSymbol "add"
            let result = SList [
                            SSymbol "define",
                            SList [
                                SSymbol "add",
                                SSymbol "a",
                                SSymbol "b"
                            ],
                            SList [
                                SSymbol "+",
                                SSymbol "a",
                                SSymbol "b"
                            ]
                        ]
            getWithDefine ast target `shouldBe` Just result

        it "returns Nothing if the target does not match any define" $ do
            let ast =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "x"],
                                    SSymbol "body"
                                ]
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "g",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "y"],
                                    SSymbol "body2"
                                ]
                            ]
                        ]
            getWithDefine ast (SSymbol "h") `shouldBe` Nothing

        it "returns Nothing if the AST is empty" $ do
            let ast = SList []
            getWithDefine ast (SSymbol "f") `shouldBe` Nothing

        it "returns the first match if multiple define blocks have the same target" $ do
            let ast =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "x"],
                                    SSymbol "body"
                                ]
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "y"],
                                    SSymbol "body2"
                                ]
                            ]
                        ]
            getWithDefine ast (SSymbol "f") `shouldBe` Just (SList [SSymbol "define", SSymbol "f", SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "body"]])

        it "handles a single define correctly" $ do
            let ast =   SList [
                            SList [
                                SSymbol "define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "x"],
                                    SSymbol "body"
                                ]
                            ]
                        ]
            getWithDefine ast (SSymbol "f") `shouldBe` Just (SList [SSymbol "define", SSymbol "f", SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "body"]])

        it "ignores non-define entries in the AST" $ do
            let ast =   SList [
                            SList [
                                SSymbol "non-define",
                                SSymbol "f",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "x"],
                                    SSymbol "body"
                                ]
                            ],
                            SList [
                                SSymbol "define",
                                SSymbol "g",
                                SList [
                                    SSymbol "lambda",
                                    SList [SSymbol "y"],
                                    SSymbol "body2"
                                ]
                            ]
                        ]
            getWithDefine ast (SSymbol "g") `shouldBe` Just (SList [SSymbol "define", SSymbol "g", SList [SSymbol "lambda", SList [SSymbol "y"], SSymbol "body2"]])
            getWithDefine ast (SSymbol "f") `shouldBe` Nothing
