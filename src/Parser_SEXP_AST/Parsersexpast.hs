module Parser_SEXP_AST.Parsersexpast (parseAST) where

import StructureSE.StructureSE (SExpr(..))
import StructureAST.StructureAST (AST(..))

parseAST :: Maybe SExpr -> Maybe AST
parseAST Nothing = Nothing
