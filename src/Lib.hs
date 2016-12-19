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

type family GetColumnsToSelect tableReference columns where
  GetColumnsToSelect tableReference columns = NormalizeColumnsList tableReference (ToList columns)

type family NormalizeColumnsList tableReference columnsList where
  NormalizeColumnsList tableReference columnsList = ReplaceInList columnsList Star (GetTableColumns tableReference)

type family ValidateSelect s where
  ValidateSelect (SELECT columns FROM tableReference ()) = Difference (GetTableColumns tableReference) (GetColumnsToSelect tableReference columns) ~ '[]

data SELECT columns from tableReference conditions = SELECT columns from tableReference conditions
data Star = Star
data FROM = FROM
from :: FROM
from = FROM

select :: ValidateSelect (SELECT columns FROM tableReference ())
       => columns
       -> FROM
       -> tableReference
       -> SELECT columns FROM tableReference ()
select columns from' tableReference = SELECT columns from' tableReference ()

where' :: forall columns from tableReference conditions e es. AppendToTuple es e conditions => e -> SELECT columns from tableReference es -> SELECT columns from tableReference conditions
where' condition (SELECT columns from' tableReference conditions) = SELECT columns from' tableReference (appendToTuple conditions condition)

data Equals = Equals
newtype Condition a = Condition a

eq :: (GetColumnType column ~ columnType) => column -> columnType -> Condition (Equals, column, columnType)
eq column value = Condition (Equals, column, value)

-- LIST FUNCTIONS
------------------------------------------------------------------------------------------------------------------------
type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = 'False
  IsElementOf x (x ': xs) = 'True
  IsElementOf x (y ': ys) = IsElementOf x ys

type family AppendLists (xs :: [k]) (ys :: [k]) where
  AppendLists '[] ys = ys
  AppendLists (x ': xs) ys = x ': (AppendLists xs ys)

type family ToListOfLists (xs :: [k]) where
  ToListOfLists xs = ToListOfListsHelper '[] xs

type family FromListOfLists (xss :: [[k]]) where
  FromListOfLists xss = FromListOfListsHelper '[] xss

type family FromListOfListsHelper (acc :: [k]) (xss :: [[k]]) where
  FromListOfListsHelper acc '[] = acc
  FromListOfListsHelper acc (xs ': xss) = FromListOfListsHelper (AppendLists acc xs) xss

type family ToListOfListsHelper (acc :: [[k]]) (xs :: [k]) where
  ToListOfListsHelper acc '[] = acc
  ToListOfListsHelper acc (x ': xs) = ToListOfListsHelper ('[x] ': acc) xs

type family ReplaceInList (xs :: [k]) (x :: k) (replacements :: [k]) where
  ReplaceInList xs x replacements = FromListOfLists (ReplaceInListHelper '[] (ToListOfLists xs) x replacements)

type family ReplaceInListHelper (acc :: [[k]]) (xss :: [[k]]) (x :: k) (replacements :: [k]) where
  ReplaceInListHelper acc '[] _ _ = acc
  ReplaceInListHelper acc ('[x] ': xss) x replacements = ReplaceInListHelper (replacements ': acc) xss x replacements
  ReplaceInListHelper acc (xs ': xss) x replacements = ReplaceInListHelper (xs ': acc) xss x replacements

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

serialize :: forall columns from tableReference conditions. (ToValues columns, ToValue tableReference, ToValues conditions) => SELECT columns from tableReference conditions -> String
serialize (SELECT columns _ tableReference conditions) = "SELECT " ++ intercalate ", " (toValues columns) ++ " FROM " ++ toValue tableReference ++ conditions'
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
instance ToValue Star where
  toValue _ = "*"
instance ToValue Users where
  toValue _ = "users"
instance ToValue Name where
  toValue _ = "name"
instance ToValue Age where
  toValue _ = "email"
instance ToValue Author where
  toValue _ = "author"
instance ToValue String where
  toValue x = "'" ++ x ++ "'"
instance ToValue Integer where
  toValue = show

-- TODO: generalize.
instance (ToValue column, ToValue value) => ToValue (Condition (Equals, column, value)) where
  toValue (Condition (Equals, column, value)) = toValue column ++ " = " ++ toValue value


-- SCHEMA
-- currently only column names.
------------------------------------------------------------------------------------------------------------------------
type family GetTableColumns tableReference where
  GetTableColumns Users = '[Name, Age]
  GetTableColumns Comments = '[Author]

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
