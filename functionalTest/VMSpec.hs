module VMSpec (spec) where

import Test.Hspec
import RunVM (initializeVM, execute, executeInstructions, runVM)
import Control.Exception (evaluate)
import Structure (Value(..), BinaryOperator(..), BinaryComparator(..), Instruction(..), VM(..))
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "VM execution" $ do

    -- Test de base pour la pile
    it "should push and pop values on the stack correctly" $ do
      let vm = (initializeVM [STORE_CONST (VInt 10), STORE_CONST (VInt 20)]) { stack = [] }
      let vm1 = execute (STORE_CONST (VInt 10)) vm
      stack vm1 `shouldBe` [VInt 10]
      let vm2 = execute (STORE_CONST (VInt 20)) vm1
      stack vm2 `shouldBe` [VInt 20, VInt 10]

    -- Test des variables
    it "should store and retrieve variables correctly" $ do
      let vm = (initializeVM [STORE_VAR "a", LOAD_VAR "a"]) { stack = [VInt 100] }
      let vm1 = execute (STORE_VAR "a") vm
      variables vm1 `shouldBe` Map.singleton "a" (VInt 100)
      let vm2 = execute (LOAD_VAR "a") vm1
      stack vm2 `shouldBe` [VInt 100]

    -- Test des opérations binaires
    it "should perform addition correctly" $ do
      let vm = (initializeVM [OPERATOR ADD]) { stack = [VInt 4, VInt 6] }
      let vm1 = execute (OPERATOR ADD) vm
      stack vm1 `shouldBe` [VInt 10]

    it "should perform division correctly" $ do
      let vm = (initializeVM [OPERATOR DIVIDE]) { stack = [VInt 3, VInt 12] }
      let vm1 = execute (OPERATOR DIVIDE) vm
      stack vm1 `shouldBe` [VInt 4]

    it "throws an error for division by zero" $ do
      let vm = initializeVM [STORE_CONST (VInt 1), STORE_CONST (VInt 0)]
      evaluate (execute (OPERATOR DIVIDE) vm) `shouldThrow` anyErrorCall

    -- Test des comparateurs
    it "should compare integers correctly" $ do
      let vm = (initializeVM [COMPARATOR COMPARE_GT]) { stack = [VInt 2, VInt 5] }
      let vm1 = execute (COMPARATOR COMPARE_GT) vm
      stack vm1 `shouldBe` [VBool True]

    -- Test des labels et des sauts
    it "should jump to a label correctly" $ do
      let vm = initializeVM
            [ LABEL "start"
            , STORE_CONST (VInt 42)
            , JUMP "end"
            , LABEL "end"
            , HALT]
      let vm1 = executeInstructions vm
      index vm1 `shouldBe` -1
      stack vm1 `shouldBe` [VInt 42]

    it "should perform conditional jumps correctly" $ do
      let vm = initializeVM
            [ STORE_CONST (VBool False)
            , JUMP_IF_FALSE "skip"
            , STORE_CONST (VInt 99)
            , LABEL "skip"
            , HALT]
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` []

    -- -- Test des fonctions
    -- it "should call and return from functions correctly" $ do
    --   let instructions =
    --           [ CALL "main"           -- Appelle la fonction "main" depuis le contexte global
    --           , LABEL_FUNC "main"
    --           , STORE_CONST (VInt 10) -- Empile 10
    --           , CALL "addOne"         -- Appelle la fonction "addOne"
    --           , RETURN                -- Termine l'exécution de "main"
    --           , LABEL_FUNC_END "main" -- Fin de la fonction "main"
    --           , LABEL_FUNC "addOne"   -- Début de la fonction "addOne"
    --           , STORE_CONST (VInt 1)  -- Empile 1
    --           , OPERATOR ADD          -- Ajoute 10 + 1
    --           , RETURN                -- Retourne à l'appelant avec le résultat sur la pile
    --           , LABEL_FUNC_END "addOne" -- Fin de la fonction "addOne"
    --           ]
    --   let vm = initializeVM instructions
    --   let vm1 = executeInstructions vm
    --   stack vm1 `shouldBe` [VInt 11]

    -- Test des instructions complexes
    it "should handle a complex sequence of instructions" $ do
      let instructions =
              [ STORE_CONST (VInt 5)         -- Empile 5
              , STORE_CONST (VInt 3)         -- Empile 3
              , OPERATOR MULTIPLY            -- Multiplie 5 * 3 (résultat : 15)
              , STORE_VAR "result"           -- Stocke 15 dans la variable "result"
              , LOAD_VAR "result"            -- Charge "result" sur la pile
              , STORE_CONST (VInt 5)         -- Empile 5
              , OPERATOR ADD                 -- Ajoute 15 + 5 (résultat : 20)
              , HALT                         -- Stoppe l'exécution
              ]
      let vm = initializeVM instructions
      let vm1 = executeInstructions vm
      stack vm1 `shouldBe` [VInt 20]
      variables vm1 `shouldBe` Map.singleton "result" (VInt 15)
