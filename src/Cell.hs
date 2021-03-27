module Cell where

data Cell = X | O | Empty

instance Show Cell where
  show X = "X"
  show O = "O"
  show Empty = " "
