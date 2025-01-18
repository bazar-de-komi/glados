-- | Parser.ParserSExpAST
--
-- This module provides functions for converting `SExpr` (Kleftis symbolic expressions)
-- into an `AST` (Abstract Syntax Tree) structure. These functions handle
-- conditional expressions, loops, definitions, and other language-specific constructs.

module Parser.ParserSExpAST (lastCheck, parseFinalAST, findFuncOrDef, findIf, findLoop, findFor) where

import Structure (SExpr(..), AST(..))

-- | `findFuncOrDef` - Parses a list of `SExpr` to identify a function or definition.
--
-- The function handles both:
-- - Functions with parameters and a body.
-- - Simple variable or type definitions.
--
-- ==== Parameters:
-- - `[SExpr]`: A list of symbolic expressions representing a function or definition.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure representing the function or definition.
-- - `Left String`: An error if the input does not match the expected patterns.
--
-- ==== Example:
-- >>> findFuncOrDef [Atom "add", Type "int", Param [Atom "a", Atom "b"], List [BasicFunc "+", Atom "a", Atom "b"]]
-- Right (SFunc "add" (SType "int") (SList [SVariable "a", SVariable "b"]) (SList [SOperation "+", SVariable "a", SVariable "b"]))
findFuncOrDef :: [SExpr] -> Either String AST
findFuncOrDef (Atom n : Type a : Param b : List r : []) =
    case (mapM parseFinalAST b, mapM parseFinalAST r) of
      (Right paramAST, Right bodyAST) -> Right (SFunc n (SType a) (SList paramAST) (SList bodyAST))
      (Left err, _) -> Left err
      (_, Left err) -> Left err
findFuncOrDef (Atom n : Type a : Param b : r) =
    case (mapM parseFinalAST b, mapM parseFinalAST r) of
      (Right paramAST, Right bodyAST) -> Right (SFunc n (SType a) (SList paramAST) (SList bodyAST))
      (Left err, _) -> Left err
      (_, Left err) -> Left err
findFuncOrDef (Atom n : Type a : List r : []) =
    case mapM parseFinalAST r of
      Right bodyAST -> Right (SDefine n (SType a) (SList bodyAST))
      Left err -> Left err
findFuncOrDef (Atom n : Type a : r) =
    case mapM parseFinalAST r of
      Right bodyAST -> Right (SDefine n (SType a) (SList bodyAST))
      Left err -> Left err
findFuncOrDef _ = Left $ "Unsupported function or definition: "

-- | `findIf` - Parses a conditional `if ... else` expression.
--
-- This function identifies:
-- - The "if" branch (condition).
-- - The "then" branch (content).
-- - The "else" branch (alternative content).
--
-- ==== Parameters:
-- - `SExpr`: A symbolic conditional expression.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure representing the conditional expression.
-- - `Left String`: An error if the input is not recognized.
--
-- ==== Example:
-- >>> findIf (SEIf (Param [Atom "a", BasicFunc ">", Atom "b"]) (List [Atom "result", Atom "a"]) (List [Atom "result", Atom "b"]))
-- Right (SIf (SList [SVariable "a", SOperation ">", SVariable "b"]) (SList [SVariable "result", SVariable "a"]) (SList [SVariable "result", SVariable "b"]))
findIf :: SExpr -> Either String AST
findIf (SEIf (Param a) (List b) (List c)) =
    case (mapM parseFinalAST a, parseFinalAST (List b), parseFinalAST (List c)) of
      (Right paramIf, Right bodyThen, Right bodyElse ) -> Right (SIf (SList paramIf) bodyThen bodyElse)
      (Left err, _, _) -> Left err
      (_, Left err, _) -> Left err
      (_, _, Left err) -> Left err
findIf a = Left $ "Unsupported SExpr: " ++ show a

-- | `findLoop` - Parses a `while` loop expression.
--
-- This function identifies:
-- - The parameters of the loop.
-- - The body of the loop.
--
-- ==== Parameters:
-- - `SExpr`: A symbolic `while` loop expression.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure representing the loop.
-- - `Left String`: An error if the input is not recognized.
--
-- ==== Example:
-- >>> findLoop (SELoop (Param [Atom "i", BasicFunc "<", SEInt 10]) (List [BasicFunc "+=", Atom "i", SEInt 1]))
-- Right (SLoop (SList [SVariable "i", SOperation "<", SInt 10]) (SList [SOperation "+=", SVariable "i", SInt 1]))
findLoop :: SExpr -> Either String AST
findLoop (SELoop (Param a) (List b)) =
    case (mapM parseFinalAST a, parseFinalAST (List b)) of
      (Right paramIf, Right bodyThen) -> Right (SLoop (SList paramIf) bodyThen)
      (Left err, _) -> Left err
      (_, Left err) -> Left err
findLoop a = Left $ "Unsupported SExpr: " ++ show a

-- | `findFor` - Parses a `for` loop expression.
--
-- This function identifies:
-- - Initialization, condition, and increment parts of the loop.
-- - The body of the loop.
--
-- ==== Parameters:
-- - `SExpr`: A symbolic `for` loop expression.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure representing the loop.
-- - `Left String`: An error if the input is not recognized.
--
-- ==== Example:
-- >>> findFor (SEFor (List [BasicFunc "=", Atom "i", SEInt 0]) (Param [Atom "i", BasicFunc "<", SEInt 10]) (List [BasicFunc "+=", Atom "i", SEInt 1]) (List [Atom "print", Atom "i"]))
-- Right (SFor (SList [SOperation "=", SVariable "i", SInt 0]) (SList [SVariable "i", SOperation "<", SInt 10]) (SList [SOperation "+=", SVariable "i", SInt 1]) (SList [SVariable "print", SVariable "i"]))
findFor :: SExpr -> Either String AST
findFor (SEFor (List d) (Param a) (List b) (List c)) =
    case (mapM parseFinalAST a, parseFinalAST (List b), parseFinalAST (List c), parseFinalAST (List d)) of
      (Right paramIf, Right bodyThen, Right bodyElse, Right initfor) -> Right (SFor initfor (SList paramIf) bodyThen bodyElse)
      (Left err, _, _, _) -> Left err
      (_, Left err, _, _) -> Left err
      (_, _, Left err, _) -> Left err
      (_, _, _, Left err) -> Left err
findFor a = Left $ "Unsupported SExpr: " ++ show a

-- | `findFor` - Parses a `for` loop expression.
--
-- This function identifies:
-- - name of the function, parameter given to it.
--
-- ==== Parameters:
-- - `SExpr`: A symbolic `List` call expression.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure representing the call.
-- - `Left String`: An error if the input is not recognized.
--
-- ==== Example:
-- >>> findCall (List [Atom "name", Slist[SEInt 1]])
-- Right (SCall "name" (SList[SInt 1]))
findCall :: SExpr -> Either String AST
findCall (List (Atom a : List b : [])) =
    case (a, parseFinalAST (List b)) of
      (name, Right paramAST) -> Right (SCall name paramAST)
      (_, Left err) -> Left err
findCall a = Left $ "Unsupported SExpr: " ++ show a

-- | `parseFinalAST` - Converts a symbolic expression (`SExpr`) into an `AST` structure.
--
-- This function handles a variety of expression types, including:
-- - Booleans (`Boolean`).
-- - Floats (`SEFloat`).
-- - Integers (`SEInt`).
-- - Strings (`SEString`).
-- - Functions (`SFunc`).
-- - Definitions (`SDefine`).
-- - Conditional structures (`SEIf`).
-- - Loops (`SELoop`, `SEFor`).
--
-- ==== Parameters:
-- - `SExpr`: The symbolic expression to convert.
--
-- ==== Returns:
-- - `Right AST`: An `AST` structure corresponding to the input.
-- - `Left String`: An error if the input is unrecognized or invalid.
--
-- ==== Example:
-- >>> parseFinalAST (SEInt 42)
-- Right (SInt 42)
-- >>> parseFinalAST (SEIf (Param [Atom "a", BasicFunc ">", Atom "b"]) (List [Atom "result", Atom "a"]) (List [Atom "result", Atom "b"]))
-- Right (SIf (SList [SVariable "a", SOperation ">", SVariable "b"]) (SList [SVariable "result", SVariable "a"]) (SList [SVariable "result", SVariable "b"]))
parseFinalAST :: SExpr -> Either String AST
parseFinalAST (Boolean a) = Right (SBool a)
parseFinalAST (SEFloat a) = Right (SFloat a)
parseFinalAST (SEInt a) = Right (SInt a)
parseFinalAST (SEChar a) = Right (SChar a)
parseFinalAST (Type a) = Right (SType a)
parseFinalAST (SEString a) = Right (SString a)
parseFinalAST (Atom a) = Right (SVariable a)
parseFinalAST (BasicFunc a) = Right (SOperation a)
parseFinalAST (SEIf a b c) = findIf (SEIf a b c)
parseFinalAST (SELoop a b) = findLoop (SELoop a b)
parseFinalAST (SEFor a b c d) = findFor (SEFor a b c d)
parseFinalAST (Return a) =
    case parseFinalAST a of
      Right astList -> Right (SReturn astList)
      Left err -> Left err
parseFinalAST (List (Atom n : BasicFunc ":" : rest)) = findFuncOrDef (Atom n : rest)
parseFinalAST (List (Atom n : List a: [])) = findCall (List (Atom n : List a: []))
parseFinalAST (List a) =
    case mapM parseFinalAST a of
      Right astList -> Right (SList astList)
      Left err -> Left err
parseFinalAST (SEList a) =
    case mapM parseFinalAST a of
      Right astList -> Right (SListOf astList)
      Left err -> Left err
parseFinalAST expr = Left $ "Unsupported SExpr: " ++ show expr

-- | `checkFinalAST` - Validates the entire AST for duplicate function or definition names.
--
-- This function iterates through a list of `AST` nodes to check if there are any
-- duplicate function (`SFunc`) or definition (`SDefine`) names. It collects all errors
-- in the form of strings.
--
-- ==== Parameters:
-- - `[AST]`: A list of `AST` nodes to be validated.
--
-- ==== Returns:
-- - `String`: An error message if duplicates are found; an empty string otherwise.
--
-- ==== Example:
-- >>> checkFinalAST [SFunc "test" (...) (...), SFunc "test" (...) (...)]
-- "ERROR test is initialized 2 times"
-- >>> checkFinalAST [SFunc "test1" (...) (...), SFunc "test2" (...) (...)]
-- ""
checkFinalAST :: [AST] -> String
checkFinalAST [] = ""
checkFinalAST ((SFunc name _ _ _) : b) = chekInAST b name ++ checkFinalAST b
checkFinalAST ((SDefine name _ _) : b) = chekInAST b name ++ checkFinalAST b
checkFinalAST ((SList a) : b) = checkFinalAST a ++ checkFinalAST b
checkFinalAST ((SVariable a) : (SVariable b) : _) = "ERROR : " ++ a ++ " " ++ b ++ " with no operation"
checkFinalAST (_ : b) = checkFinalAST b

-- | `chekInAST` - Validates that a specific name is not duplicated in the AST.
--
-- This function checks if a function or definition name appears multiple times
-- within the remaining nodes of the AST.
--
-- ==== Parameters:
-- - `[AST]`: A list of `AST` nodes to search.
-- - `String`: The name to check for duplicates.
--
-- ==== Returns:
-- - `String`: An error message if the name is duplicated; an empty string otherwise.
--
-- ==== Example:
-- >>> chekInAST [SFunc "test" (...) (...), SFunc "test" (...) (...)] "test"
-- "ERROR test is initialized 2 times"
-- >>> chekInAST [SFunc "test1" (...) (...), SFunc "test2" (...) (...)] "test1"
-- ""
chekInAST :: [AST] -> String -> String
chekInAST [] _ = ""
chekInAST ((SFunc name _ _ _) : b) str
  | name == str = "ERROR " ++ name ++ " is initialized 2 times"
  | otherwise = chekInAST b str
chekInAST ((SDefine name _ _) : b) str
  | name == str = "ERROR " ++ name ++ " is initialized 2 times"
  | otherwise = chekInAST b str
chekInAST (_ : b) str = chekInAST b str

-- | `lastCheck` - Validates the final AST for any structural errors.
--
-- This function performs a final validation step on the entire AST, ensuring
-- that there are no unexpected nodes or duplicate names.
--
-- ==== Parameters:
-- - `Either String AST`: The result of the AST conversion process, which may
--   contain a valid `AST` or an error message.
--
-- ==== Returns:
-- - `Either String AST`: Returns the validated `AST` if no errors are found,
--   or an error message if the validation fails.
--
-- ==== Example:
-- >>> lastCheck (Right (SList [SFunc "test" (...) (...)]))
-- Right (SList [SFunc "test" (...) (...)])
-- >>> lastCheck (Right (SList [SFunc "test" (...) (...), SFunc "test" (...) (...)]))
-- Left "ERROR test is initialized 2 times"
-- >>> lastCheck (Right (SEInt 42))
-- Left "ERROR Unexpected AST :(SEInt 42)"
lastCheck :: Either String AST -> Either String AST
lastCheck (Right (SList a))
  | checkFinalAST a == "" = Right (SList a)
  | otherwise = Left (checkFinalAST a)
lastCheck (Right a) = Left ("ERROR Unexpected AST :" ++ show a)
lastCheck (Left a) = Left a
