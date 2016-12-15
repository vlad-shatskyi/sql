module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ serialize $ select (C2 Name Email) Users

serialize :: SELECT columns table -> String
serialize (SELECT (C2 first second') table) = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " [map toLower (show first), map toLower (show second')]
        tableName  = map toLower $ show table

select = SELECT

class Show a => IsTable a
class Show a => IsColumn a
class (IsTable table, IsColumn column) => HasColumn table column

data Users = Users deriving Show
data Name = Name deriving Show
data Email = Email deriving Show

instance IsTable Users
instance IsColumn Name
instance IsColumn Email
instance HasColumn Users Name
instance HasColumn Users Email


data C2 a b where
  C2 :: (IsColumn a, IsColumn b) => a -> b -> C2 a b


data SELECT columns table where
  SELECT :: (IsTable table, HasColumn table column1, HasColumn table column2) => C2 column1 column2 -> table -> SELECT (C2 column1 column2) table
