module ConstAndVarTable (
    ConstAndVarTable,
    emptyTable,
    storeConst,
    storeVar,
    loadVar,
    varExists
) where

import qualified Data.Map as Map

-- Définir une table de constantes et de variables comme une Map
-- String : nom de la variable ou constante, a : type de la valeur
newtype ConstAndVarTable a = ConstAndVarTable (Map.Map String a)
  deriving (Show, Eq)

emptyTable :: ConstAndVarTable a
emptyTable = ConstAndVarTable Map.empty

storeConst :: String -> a -> ConstAndVarTable a -> ConstAndVarTable a
storeConst name value (ConstAndVarTable table) = ConstAndVarTable (Map.insert name value table)

storeVar :: String -> a -> ConstAndVarTable a -> ConstAndVarTable a
storeVar = storeConst

loadVar :: String -> ConstAndVarTable a -> Either String a
loadVar name (ConstAndVarTable table) =
    case Map.lookup name table of
        Just value -> Right value
        Nothing    -> Left $ "Erreur : La variable ou constante '" ++ name ++ "' n'existe pas."

varExists :: String -> ConstAndVarTable a -> Bool
varExists name (ConstAndVarTable table) = Map.member name table
