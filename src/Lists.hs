module Lists where

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
