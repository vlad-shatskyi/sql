module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Data.Type.List (Map)

someFunc :: IO ()
someFunc = putStrLn "bar"
-- someFunc = putStrLn $ serialize $ select (Name, Email) Users

-- serialize :: SELECT columns table -> String
-- serialize (SELECT group table) = sql
--   where sql = [ "SELECT " ++ columnsSQL
--               , "FROM " ++ tableName
--               ] & intercalate "\n"
--         columnsSQL = intercalate ", " (map (map toLower) (showColumnGroup group))
--         tableName  = map toLower $ show table

data Users = Users deriving Show
data Name = Name deriving Show
data Email = Email deriving Show

data Comments = Comments deriving Show
data Author = Author deriving Show


type AllColumnsExist (passed :: [t]) (onTable :: [t]) = '[True, True] ~ '[True, True]

select :: AllColumnsExist (ToList columns) (GetColumns table)
       => columns
       -> table
       -> ToProxy (ToList columns)
select = undefined


mySelect = select (Name, Email) Users





type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = False
  IsElementOf x (x ': xs) = True
  IsElementOf x (y ': ys) = IsElementOf x ys

type family Insert a xs where
  Insert a '[] = (a ': '[])

type family Unite a b where
  Unite (Proxy a) (Proxy b) = Proxy '[a, b]

type family GetColumns table where
  GetColumns Users = '[Name, Email]
  GetColumns Comments = '[Author]

type family ToList tuple where
  ToList (a, b) = '[a, b]

type family ToProxy a where
  ToProxy a = Proxy a

type family Head x where
  Head (x ': xs) = x
