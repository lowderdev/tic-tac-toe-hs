module Cell where

data Cell = X | O | Empty
  deriving (Eq)

instance Show Cell where
  show X = "X"
  show O = "O"
  show Empty = " "
