module Tuples where

newtype OneTuple a = OneTuple a

class ConsTuple e es ees | e es -> ees where
  consTuple :: e -> es -> ees

instance ConsTuple e () (OneTuple e) where
  consTuple e _ = OneTuple e
instance ConsTuple e1 (OneTuple e2) (e1, e2) where
  consTuple e1 (OneTuple e2) = (e1, e2)
instance ConsTuple e1 (e2, e3) (e1, e2, e3) where
  consTuple e1 (e2, e3) = (e1, e2, e3)
instance ConsTuple e1 (e2, e3, e4) (e1, e2, e3, e4) where
  consTuple e1 (e2, e3, e4) = (e1, e2, e3, e4)
