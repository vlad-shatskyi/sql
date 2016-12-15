module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ select [Name] Users

select :: (Column column, Table table) => [column] -> table -> String
select columns table = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map columnName columns)
        columnName = map toLower . show
        tableName  = map toLower $ show table

class Show a => Table a
class Show a => Column a

data Users = Users deriving Show
data Name = Name deriving Show

instance Table Users
instance Column Name
