module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Type.List (Difference)
import Tuples (OneTuple(OneTuple), AppendToTuple, appendToTuple)

-- LIBRARY.
------------------------------------------------------------------------------------------------------------------------
class ToValues a where
  toValues :: a -> [String]
class ToValue a where
  toValue :: a -> String

type family ValidateSelect s where
  ValidateSelect (SELECT columns FROM table ()) = Difference (ToList columns) (GetColumns table) ~ '[]

data SELECT columns from table conditions = SELECT columns from table conditions
data FROM = FROM
from :: FROM
from = FROM

select :: ValidateSelect (SELECT columns FROM table ())
       => columns
       -> FROM
       -> table
       -> SELECT columns FROM table ()
select columns from' table = SELECT columns from' table ()

where' :: forall columns from table conditions e es. AppendToTuple es e conditions => e -> SELECT columns from table es -> SELECT columns from table conditions
where' condition (SELECT columns from' table conditions) = SELECT columns from' table (appendToTuple conditions condition)

data Equals = Equals
newtype Condition a = Condition a

eq :: (GetColumnType column ~ columnType) => column -> columnType -> Condition (Equals, column, columnType)
eq column value = Condition (Equals, column, value)

type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = 'False
  IsElementOf x (x ': xs) = 'True
  IsElementOf x (y ': ys) = IsElementOf x ys

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

type family ListPrepend x xs where
  ListPrepend x xs = x ': xs

type family Head x where
  Head (x ': xs) = x

serialize :: forall columns from table conditions. (ToValues columns, ToValue table, ToValues conditions) => SELECT columns from table conditions -> String
serialize (SELECT columns _ table conditions) = "SELECT " ++ intercalate ", " (toValues columns) ++ " FROM " ++ toValue table ++ conditions'
  where conditions' = case toValues conditions of
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
data Age = Age

-- comments columns
data Author = Author deriving Show


-- boilerplate
-- TODO: Generate via TH or find a way to derive from `data` somehow.

instance ToValues () where
  toValues _ = []

instance (ToValue v1) => ToValues (OneTuple v1) where
  toValues (OneTuple v1) = [toValue v1]
instance (ToValue v1, ToValue v2) => ToValues (v1, v2) where
  toValues (v1, v2) = [toValue v1, toValue v2]
instance (ToValue v1, ToValue v2, ToValue v3) => ToValues (v1, v2, v3) where
  toValues (v1, v2, v3) = [toValue v1, toValue v2, toValue v3]
instance ToValue Users where
  toValue _ = "users"
instance ToValue Name where
  toValue _ = "name"
instance ToValue Age where
  toValue _ = "email"
instance ToValue Author where
  toValue _ = "author"
instance ToValue String where
  toValue s = "'" ++ s ++ "'"
instance ToValue Integer where
  toValue = show

-- TODO: generalize.
instance (ToValue column, ToValue value) => ToValue (Condition (Equals, column, value)) where
  toValue (Condition (Equals, column, value)) = toValue column ++ " = " ++ toValue value


-- SCHEMA
-- currently only column names.
------------------------------------------------------------------------------------------------------------------------
type family GetColumns table where
  GetColumns Users = '[Name, Age]
  GetColumns Comments = '[Author]

type family GetColumnType column where
  GetColumnType Name = String
  GetColumnType Age = Integer


-- s = select (Name, Age) from Users -- should compile.
-- s = select (Name, Author) from Users -- should not compile because there is no Author in Users.
-- s = (select (Name, Age) (select (Name) from Users)) -- should not compile because there is no Age in the inner select.
-- s =  select (Name, Age) from Users & where' (Name `eq` "john") & where' (Age `eq` "18")-- should not compile because integer column Age is compared with a string.
s =  select (Name, Age) from Users & where' (Name `eq` "john") & where' (Age `eq` 18)-- should compile.


someFunc :: IO ()
someFunc = (putStrLn . serialize) s
