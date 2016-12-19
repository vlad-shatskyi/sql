module Lib
    ( someFunc
    ) where

import Lists
import Tuples

import Data.List (intercalate)
import Data.Function ((&))
import Data.Type.List (Difference)
import Data.Kind (Type)
import GHC.TypeLits

-- LIBRARY.
------------------------------------------------------------------------------------------------------------------------
class ToSQLStringList a where
  toSQLStringList :: a -> [String]
class ToSQLString a where
  toSQLString :: a -> String

type family GetSelectList tableReference selectList where
  GetSelectList tableReference selectList = NormalizeSelectList tableReference (ToList selectList)

type family NormalizeSelectList tableReference (selectList :: [Type]) where
  NormalizeSelectList tableReference selectList = ReplaceInList selectList Asterisk (GetAllColumns tableReference)

type family ExtraColumnsError extraColumns allColumns where
  ExtraColumnsError '[] _ = 'True ~ 'True
  ExtraColumnsError '[extraColumn] allColumns = TypeError ('Text "Column "  ':<>: 'ShowType extraColumn  ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)
  ExtraColumnsError extraColumns   allColumns = TypeError ('Text "Columns " ':<>: 'ShowType extraColumns ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)

type family ValidateSelectListType selectList where
  ValidateSelectListType [x] = TypeError ('Text "Please use tuples instead of a list to specify the columns to select.")
  ValidateSelectListType other = 'True ~ 'True

type family ValidateSelect s where
  ValidateSelect (SELECT selectList FROM tableReference ()) =
    ( ValidateSelectListType selectList
    , ExtraColumnsError (Difference (GetAllColumns tableReference) (GetSelectList tableReference selectList)) (GetAllColumns tableReference)
    )

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

newtype Condition a = Condition a
data Equals = Equals

eq :: (GetColumnType column ~ columnType) => column -> columnType -> Condition (Equals, column, columnType)
eq column value = Condition (Equals, column, value)

serialize :: forall selectList from tableReference conditions. (ToSQLStringList selectList, ToSQLString tableReference, ToSQLStringList conditions) => SELECT selectList from tableReference conditions -> String
serialize (SELECT selectList _ tableReference conditions) = "SELECT " ++ intercalate ", " (toSQLStringList selectList) ++ " FROM " ++ toSQLString tableReference ++ conditions'
  where conditions' = case toSQLStringList conditions of
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
instance ToSQLStringList () where
  toSQLStringList _ = []
instance {-# OVERLAPPABLE #-} ToSQLString v1 => ToSQLStringList v1 where
  toSQLStringList v1 = [toSQLString v1]
instance (ToSQLString v1) => ToSQLStringList (OneTuple v1) where
  toSQLStringList (OneTuple v1) = [toSQLString v1]
instance (ToSQLString v1, ToSQLString v2) => ToSQLStringList (v1, v2) where
  toSQLStringList (v1, v2) = [toSQLString v1, toSQLString v2]
instance (ToSQLString v1, ToSQLString v2, ToSQLString v3) => ToSQLStringList (v1, v2, v3) where
  toSQLStringList (v1, v2, v3) = [toSQLString v1, toSQLString v2, toSQLString v3]


instance ToSQLString Asterisk where
  toSQLString _ = "*"
instance ToSQLString Users where
  toSQLString _ = "users"
instance ToSQLString Name where
  toSQLString _ = "name"
instance ToSQLString Age where
  toSQLString _ = "email"
instance ToSQLString Author where
  toSQLString _ = "author"
instance ToSQLString String where
  toSQLString x = "'" ++ x ++ "'"
instance ToSQLString Integer where
  toSQLString = show
instance (ToSQLStringList selectList, ToSQLString tableReference, ToSQLStringList conditions) => ToSQLString (SELECT selectList FROM tableReference conditions) where
  toSQLString select' = "(" ++ serialize select' ++ ")"

-- TODO: generalize.
instance (ToSQLString column, ToSQLString value) => ToSQLString (Condition (Equals, column, value)) where
  toSQLString (Condition (Equals, column, value)) = toSQLString column ++ " = " ++ toSQLString value


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
