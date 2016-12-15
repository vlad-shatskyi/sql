module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ select [Name] Users

select :: (IsColumn column, IsTable table, HasColumn table column) => [column] -> table -> String
select columns table = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map columnName columns)
        columnName = map toLower . show
        tableName  = map toLower $ show table

class Show a => IsTable a
class Show a => IsColumn a
class (IsTable table, IsColumn column) => HasColumn table column

data Users = Users deriving Show
data Name = Name deriving Show

instance IsTable Users
instance IsColumn Name
instance HasColumn Users Name
