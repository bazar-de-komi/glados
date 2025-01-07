module Execute (execute) where

-- import Data.Either (either)

data Function = Function
    {
        fName :: String,
        fArgs :: [Arg],
        fLines :: [String]
    } deriving (Show)

data Arg = IntArg Int
         | FloatArg Float
         | StringArg String
         | CharArg Char
         | BoolArg Bool
         deriving (Show, Eq)

execute :: String -> IO ()
execute str = on fait quoi putain ? (ctrl+c ctrl+v du bootstrap)
