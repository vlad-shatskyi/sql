module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ serialize $ select (C1 Name) Users

serialize :: SELECT columns table -> String
serialize (SELECT group table) = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map (map toLower) (showColumnGroup group))
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


data ColumnGroup where
  C1 :: forall a. IsColumn a => a -> ColumnGroup

showColumnGroup :: ColumnGroup -> [String]
showColumnGroup = \x -> case x of
  C1 a -> [show a]


data SELECT columns table where
  SELECT :: (IsTable table) => ColumnGroup -> table -> SELECT ColumnGroup table
