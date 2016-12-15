module Lib
    ( someFunc
    ) where

import Data.List (intercalate)
import Data.Function ((&))
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Data.Type.List (Difference)

someFunc :: IO ()
someFunc = putStrLn $ serialize mySelect
-- someFunc = putStrLn $ serialize $ select (Name, Email) Users

-- serialize :: SELECT columns table -> String
-- serialize (SELECT group table) = sql
--   where sql = [ "SELECT " ++ columnsSQL
--               , "FROM " ++ tableName
--               ] & intercalate "\n"
--         columnsSQL = intercalate ", " (map (map toLower) (showColumnGroup group))
--         tableName  = map toLower $ show table

data Users = Users
data Name = Name
data Email = Email

data Comments = Comments deriving Show
data Author = Author deriving Show

class ToValue a where
  toValue :: Proxy a -> String

class ToValues a where
  toValues :: Proxy a -> [String]

instance ToValue Name where
  toValue = const "name"

instance ToValue Email where
  toValue = const "email"

instance (ToValue v1, ToValue v2) => ToValues (v1, v2) where
  toValues = const [toValue (Proxy :: Proxy Name), toValue (Proxy :: Proxy Email)]

type AllColumnsExist (passed :: [t]) (onTable :: [t]) = Difference onTable passed ~ '[]

select :: AllColumnsExist (ToList columns) (GetColumns table)
       => columns
       -> table
       -> ToProxy columns
select _ _ = Proxy

mySelect = select (Name, Email) Users

serialize :: forall n proxy. ToValues n => Proxy n -> String
serialize = intercalate ", " . toValues

type family IsElementOf (x :: k) (xs :: [k]) where
  IsElementOf x '[] = False
  IsElementOf x (x ': xs) = True
  IsElementOf x (y ': ys) = IsElementOf x ys

type family Insert a xs where
  Insert a '[] = (a ': '[])

type family Unite a b where
  Unite (Proxy a) (Proxy b) = Proxy '[a, b]

type family GetColumns table where
  GetColumns Users = '[Name, Email, Author]
  GetColumns Comments = '[Author]

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
