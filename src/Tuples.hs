module Tuples where

newtype OneTuple a = OneTuple a

class AppendToTuple es e ese | es e -> ese where
  appendToTuple :: es -> e -> ese

instance AppendToTuple () e (OneTuple e) where
  appendToTuple _ = OneTuple
instance AppendToTuple (OneTuple e1) e2  (e1, e2) where
  appendToTuple (OneTuple e1) e2 = (e1, e2)
instance AppendToTuple (e1, e2) e3 (e1, e2, e3) where
  appendToTuple (e1, e2) e3 = (e1, e2, e3)
instance AppendToTuple (e1, e2, e3) e4 (e1, e2, e3, e4) where
  appendToTuple (e1, e2, e3) e4 = (e1, e2, e3, e4)
