module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Type.List (Difference)
import Data.Kind (Type)
import Tuples
import GHC.TypeLits

-- LIBRARY.
------------------------------------------------------------------------------------------------------------------------
class ToValues a where
  toValues :: a -> [String]
class ToValue a where
  toValue :: a -> String

type family GetSelectList tableReference selectList where
  GetSelectList tableReference selectList = NormalizeColumnsList tableReference (ToList selectList)

type family NormalizeColumnsList tableReference (selectList :: [Type]) where
  NormalizeColumnsList tableReference selectList = ReplaceInList selectList Asterisk (GetAllColumns tableReference)

type family ExtraColumnsError extraColumns allColumns where
  ExtraColumnsError '[] _ = 'True ~ 'True
  ExtraColumnsError '[extraColumn] allColumns = TypeError ('Text "Column "  ':<>: 'ShowType extraColumn  ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)
  ExtraColumnsError extraColumns   allColumns = TypeError ('Text "Columns " ':<>: 'ShowType extraColumns ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)

type family ValidateSelect s where
  ValidateSelect (SELECT selectList FROM tableReference ()) = ExtraColumnsError (Difference (GetAllColumns tableReference) (GetSelectList tableReference selectList)) (GetAllColumns tableReference)

data SELECT selectList from tableReference conditions = SELECT selectList from tableReference conditions
data Asterisk = Asterisk
everything :: Asterisk
everything = Asterisk
data FROM = FROM
from :: FROM
from = FROM

select :: ValidateSelect (SELECT selectList FROM tableReference ())
       => selectList
       -> FROM
       -> tableReference
       -> SELECT selectList FROM tableReference ()
select selectList from' tableReference = SELECT selectList from' tableReference ()

where' :: forall selectList from tableReference conditions e es. AppendToTuple es e conditions => e -> SELECT selectList from tableReference es -> SELECT selectList from tableReference conditions
where' condition (SELECT selectList from' tableReference conditions) = SELECT selectList from' tableReference (appendToTuple conditions condition)

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

type family ListPrepend x xs where
  ListPrepend x xs = x ': xs

type family Head x where
  Head (x ': xs) = x

serialize :: forall selectList from tableReference conditions. (ToValues selectList, ToValue tableReference, ToValues conditions) => SELECT selectList from tableReference conditions -> String
serialize (SELECT selectList _ tableReference conditions) = "SELECT " ++ intercalate ", " (toValues selectList) ++ " FROM " ++ toValue tableReference ++ conditions'
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

instance {-# OVERLAPPABLE #-} ToValue v1 => ToValues v1 where
  toValues v1 = [toValue v1]
instance (ToValue v1) => ToValues (OneTuple v1) where
  toValues (OneTuple v1) = [toValue v1]
instance (ToValue v1, ToValue v2) => ToValues (v1, v2) where
  toValues (v1, v2) = [toValue v1, toValue v2]
instance (ToValue v1, ToValue v2, ToValue v3) => ToValues (v1, v2, v3) where
  toValues (v1, v2, v3) = [toValue v1, toValue v2, toValue v3]
instance ToValue Asterisk where
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
instance (ToValues selectList, ToValue tableReference, ToValues conditions) => ToValue (SELECT selectList FROM tableReference conditions) where
  toValue select = "(" ++ serialize select ++ ")"

-- TODO: generalize.
instance (ToValue column, ToValue value) => ToValue (Condition (Equals, column, value)) where
  toValue (Condition (Equals, column, value)) = toValue column ++ " = " ++ toValue value


-- SCHEMA
-- currently only column names.
------------------------------------------------------------------------------------------------------------------------
type family GetAllColumns tableReference where
  GetAllColumns Users = '[Name, Age]
  GetAllColumns Comments = '[Author]
  GetAllColumns (SELECT selectList FROM tableReference conditions) = GetSelectList tableReference selectList

type family GetColumnType column where
  GetColumnType Name = String
  GetColumnType Age = Integer


-- s = select (Name, Age) from Users -- should compile.
-- s = select (Name, Author) from Users -- should not compile because there is no Author in Users.
-- s = select (Name, Age) from Users & where' (Name `eq` "john") & where' (Age `eq` "18")-- should not compile because integer column Age is compared with a string.
-- s = select (Name, Age) from Users & where' (Name `eq` "john") & where' (Age `eq` 18)-- should compile.
-- s = select Age from (select Name from Users) -- should not compile because there is no Age in the inner select.
s = select Age from (select everything from Users) -- should compile.


someFunc :: IO ()
someFunc = (putStrLn . serialize) s
