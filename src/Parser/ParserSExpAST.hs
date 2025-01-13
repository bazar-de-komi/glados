module Parser.ParserSExpAST (parseFinalAST) where

import Structure (SExpr(..), AST(..))

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

findIf :: SExpr -> Either String AST
findIf (SEIf (Param a) (List b) (List c)) =
    case (mapM parseFinalAST a, parseFinalAST (List b), parseFinalAST (List c)) of
      (Right paramIf, Right bodyThen, Right bodyElse ) -> Right (SIf (SList paramIf) bodyThen bodyElse)
      (Left err, _, _) -> Left err
      (_, Left err, _) -> Left err
      (_, _, Left err) -> Left err
findIf a = Left $ "Unsupported SExpr: " ++ show a

findLoop :: SExpr -> Either String AST
findLoop (SELoop (Param a) (List b)) =
    case (mapM parseFinalAST a, parseFinalAST (List b)) of
      (Right paramIf, Right bodyThen) -> Right (SLoop (SList paramIf) bodyThen)
      (Left err, _) -> Left err
      (_, Left err) -> Left err
findLoop a = Left $ "Unsupported SExpr: " ++ show a

findFor :: SExpr -> Either String AST
findFor (SEFor (List d) (Param a) (List b) (List c)) =
    case (mapM parseFinalAST a, parseFinalAST (List b), parseFinalAST (List c), parseFinalAST (List d)) of
      (Right paramIf, Right bodyThen, Right bodyElse, Right initfor) -> Right (SFor initfor (SList paramIf) bodyThen bodyElse)
      (Left err, _, _, _) -> Left err
      (_, Left err, _, _) -> Left err
      (_, _, Left err, _) -> Left err
      (_, _, _, Left err) -> Left err
findFor a = Left $ "Unsupported SExpr: " ++ show a

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
parseFinalAST (List a) =
    case mapM parseFinalAST a of
      Right astList -> Right (SList astList)
      Left err -> Left err
parseFinalAST (SEList a) =
    case mapM parseFinalAST a of
      Right astList -> Right (SListOf astList)
      Left err -> Left err
parseFinalAST expr = Left $ "Unsupported SExpr: " ++ show expr