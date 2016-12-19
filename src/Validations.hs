module Validations where

import GHC.TypeLits

type family ExtraColumnsError extraColumns allColumns where
  ExtraColumnsError '[] _ = 'True ~ 'True
  ExtraColumnsError '[extraColumn] allColumns = TypeError ('Text "Column "  ':<>: 'ShowType extraColumn  ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)
  ExtraColumnsError extraColumns   allColumns = TypeError ('Text "Columns " ':<>: 'ShowType extraColumns ':<>: 'Text " not found" ':$$: 'Text "Available columns: " ':<>: 'ShowType allColumns)

type family ValidateSelectListType selectList where
  ValidateSelectListType [x] = TypeError ('Text "Please use tuples instead of a list to specify the columns to select.")
  ValidateSelectListType other = 'True ~ 'True
