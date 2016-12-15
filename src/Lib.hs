module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn $ serialize $ select (Name, Email) Users

serialize :: SELECT columns table -> String
serialize (SELECT group table) = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map (map toLower) (showColumnGroup group))
        tableName  = map toLower $ show table

select columns = SELECT (toColumnGroup columns)

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
  C2 :: forall a b. (IsColumn a, IsColumn b) => a -> b -> ColumnGroup

showColumnGroup :: ColumnGroup -> [String]
showColumnGroup x = case x of
  C1 a -> [show a]
  C2 a b -> [show a, show b]


class ToColumnGroup a where
  toColumnGroup :: a -> ColumnGroup

instance (IsColumn a, IsColumn b) => ToColumnGroup (a, b) where
  toColumnGroup (a, b) = C2 a b

data SELECT columns table where
  SELECT :: (IsTable table) => ColumnGroup -> table -> SELECT ColumnGroup table
