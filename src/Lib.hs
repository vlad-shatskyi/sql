module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Type.List (Difference)
import Tuples (OneTuple, ConsTuple, consTuple)

-- LIBRARY.
------------------------------------------------------------------------------------------------------------------------
class ToValues a where
  toValues :: [String]

instance ToValues () where
  toValues = []

instance (ToValue v1) => ToValues (OneTuple v1) where
  toValues = [toValue @v1]
instance (ToValue v1, ToValue v2) => ToValues (v1, v2) where
  toValues = [toValue @v1, toValue @v2]
instance (ToValue v1, ToValue v2, ToValue v3) => ToValues (v1, v2, v3) where
  toValues = [toValue @v1, toValue @v2, toValue @v3]

type AllColumnsExist (passed :: [t]) (onTable :: [t]) = Difference onTable passed ~ '[]

data SELECT columns from table conditions = SELECT columns from table conditions
data FROM = FROM
from :: FROM
from = FROM

select :: AllColumnsExist (ToList columns) (GetColumns table)
       => columns
       -> FROM
       -> table
       -> SELECT columns FROM table ()
select columns from table = SELECT columns from table ()

where' :: forall columns from table conditions e es. ConsTuple e es conditions => e -> SELECT columns from table es -> SELECT columns from table conditions
where' condition (SELECT columns from table conditions) = SELECT columns from table (consTuple condition conditions)

data Equals = Equals
data Condition a = Condition a

eq column value = Condition (Equals, column, value)

type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = 'False
  IsElementOf x (x ': xs) = 'True
  IsElementOf x (y ': ys) = IsElementOf x ys

type TuplePrepend x xs = FromList (ListPrepend x (ToList xs))

-- TODO: Use HList.
type family ToList tuple where
  ToList () = '[]
  ToList (OneTuple v1) = '[v1]
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

type family FromList list where
  FromList '[] = ()
  FromList '[v1] = OneTuple v1
  FromList '[v1, v2] = (v1, v2)
  FromList '[v1, v2, v3] = (v1, v2, v3)
  FromList '[v1, v2, v3, v4] = (v1, v2, v3, v4)
  FromList '[v1, v2, v3, v4, v5] = (v1, v2, v3, v4, v5)
  FromList '[v1, v2, v3, v4, v5, v6] = (v1, v2, v3, v4, v5, v6)
  FromList '[v1, v2, v3, v4, v5, v6, v7] = (v1, v2, v3, v4, v5, v6, v7)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8] = (v1, v2, v3, v4, v5, v6, v7, v8)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9] = (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)
  FromList '[v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16] = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)

type family ListPrepend x xs where
  ListPrepend x xs = x ': xs

type family Head x where
  Head (x ': xs) = x

serialize :: forall columns from table conditions. (ToValues columns, ToValue table, ToValues conditions) => SELECT columns from table conditions -> String
serialize _ = "SELECT " ++ intercalate ", " (toValues @columns) ++ " FROM " ++ toValue @table ++ conditions
  where conditions = case toValues @conditions of
                       [] -> ""
                       xs -> " WHERE " ++ intercalate " && " xs
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

-- TODO: figure out how to have a type level integer here.
-- TODO: generalize.
instance ToValue column => ToValue (Condition (Equals, column, number)) where
  toValue = toValue @column ++ " = " ++ "12"


-- SCHEMA
-- currently only column names.
------------------------------------------------------------------------------------------------------------------------
type family GetColumns table where
  GetColumns Users = '[Name, Email]
  GetColumns Comments = '[Author]


selects =
  [ -- select (Name, Email) from Users -- should compile.
--   , select (Name, Author) from Users -- should not compile because there is no Author in Users.
--   , (select (Name, Email) (select (Name) from Users)) -- should not compile because there is no Email in the inner select.
    select (Name, Email) from Users & where' (Name `eq` 12) & where' (Email `eq` 12)-- should compile.
  ]


someFunc :: IO ()
someFunc = mapM_ (putStrLn . serialize) selects
