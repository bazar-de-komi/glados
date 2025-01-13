module VariableTable (
    VariableTable,
    emptyTable,
    writeVar,
    readVar,
    varExists
) where

import qualified Data.Map as Map

-- Définir une table de variables comme une Map
-- String : nom de la variable, a : type de la valeur
newtype VariableTable a = VariableTable (Map.Map String a)
  deriving (Show, Eq)

emptyTable :: VariableTable a
emptyTable = VariableTable Map.empty

writeVar :: String -> a -> VariableTable a -> VariableTable a
writeVar name value (VariableTable table) = VariableTable (Map.insert name value table)

readVar :: String -> VariableTable a -> Either String a
readVar name (VariableTable table) =
    case Map.lookup name table of
        Just value -> Right value
        Nothing    -> Left $ "Erreur : La variable '" ++ name ++ "' n'existe pas."

varExists :: String -> VariableTable a -> Bool
varExists name (VariableTable table) = Map.member name table
