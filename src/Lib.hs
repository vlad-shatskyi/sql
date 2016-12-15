module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))

someFunc :: IO ()
someFunc = putStrLn $ select [Column "name"] (Table "users")

select :: [Column] -> Table -> String
select columns table = sql
  where sql = [ "SELECT " ++ columnsSQL
              , "FROM " ++ tableName
              ] & intercalate "\n"
        columnsSQL = intercalate ", " (map columnName columns)
        columnName (Column name) = name
        tableName  = let (Table name) = table in name

data Table  = Table String
data Column = Column String
