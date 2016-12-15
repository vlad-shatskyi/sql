module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ serialize $ select [Name] Users

serialize :: SELECT columns table -> String
serialize (SELECT columns table) = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map columnName columns)
        columnName = map toLower . show
        tableName  = map toLower $ show table

select :: (IsColumn column, IsTable table, HasColumn table column) => [column] -> table -> SELECT column table
select = SELECT

class Show a => IsTable a
class Show a => IsColumn a
class (IsTable table, IsColumn column) => HasColumn table column

data Users = Users deriving Show
data Name = Name deriving Show

instance IsTable Users
instance IsColumn Name
instance HasColumn Users Name

data SELECT columns table where
  SELECT :: (IsColumn column, IsTable table) => [column] -> table -> SELECT column table
