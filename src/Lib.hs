module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Data.Type.List (Difference)

-- LIBRARY.
-- TODO:
------------------------------------------------------------------------------------------------------------------------
class ToValues a where
  toValues :: [String]

instance (ToValue v1, ToValue v2) => ToValues (v1, v2) where
  toValues = [toValue @v1, toValue @v2]

type AllColumnsExist (passed :: [t]) (onTable :: [t]) = Difference onTable passed ~ '[]

data FROM = FROM
from = FROM

select :: AllColumnsExist (ToList columns) (GetColumns table)
       => columns
       -> FROM
       -> table
       -> ToProxy (table, columns)
select _ _ _ = Proxy

serialize :: forall columns table. (ToValues columns, ToValue table) => Proxy (table, columns) -> String
serialize _ = "SELECT " ++ intercalate ", " (toValues @columns) ++ " FROM " ++ toValue @table

type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = 'False
  IsElementOf x (x ': xs) = 'True
  IsElementOf x (y ': ys) = IsElementOf x ys

type family Insert a xs where
  Insert a '[] = (a ': '[])

type family Unite a b where
  Unite (Proxy a) (Proxy b) = Proxy '[a, b]

type family ToList tuple where
  ToList (v1, v2) = '[v1, v2]
  ToList (v1, v2, v3) = '[v1, v2, v3]
  ToList (v1, v2, v3, v4) = '[v1, v2, v3, v4]
  ToList (v1, v2, v3, v4, v5) = '[v1, v2, v3, v4, v5]
  ToList (v1, v2, v3, v4, v5, v6) = '[v1, v2, v3, v4, v5, v6]
  ToList (v1, v2, v3, v4, v5, v6, v7) = '[v1, v2, v3, v4, v5, v6, v7]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8) = '[v1, v2, v3, v4, v5, v6, v7, v8]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15]
  ToList (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) = '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16]
  ToList v = '[v]

type family ToProxy a where
  ToProxy a = Proxy a

type family Head x where
  Head (x ': xs) = x
------------------------------------------------------------------------------------------------------------------------
-- END OF LIB.


-- DEFINITIONS.
------------------------------------------------------------------------------------------------------------------------

-- tables
data Users = Users
data Comments = Comments

-- users columns
data Name = Name
data Email = Email

-- comments columns
data Author = Author deriving Show


-- boilerplate
-- TODO: Generate via TH or find a way to derive from `data` somehow.
class ToValue a where
  toValue :: String

instance ToValue Users where
  toValue = "users"

instance ToValue Name where
  toValue = "name"

instance ToValue Email where
  toValue = "email"

instance ToValue Author where
  toValue = "author"


-- TABLE STRUCTURE.
-- currently only column names.
------------------------------------------------------------------------------------------------------------------------
type family GetColumns table where
  GetColumns Users = '[Name, Email]
  GetColumns Comments = '[Author]


selects =
  [ select (Name, Email) from Users
--   , select (Name, Author) from Users -- doesn't compile because there is no Author in Users.
--   , (select (Name, Email) (select (Name) from Users)) -- doesn't compile because there is no Email in the inner select.
  ]


someFunc :: IO ()
someFunc = mapM_ (putStrLn . serialize) selects
