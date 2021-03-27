module Grid where

import Cell

data Grid a = Grid
  { one :: a,
    two :: a,
    three :: a,
    four :: a,
    five :: a,
    six :: a,
    seven :: a,
    eight :: a,
    nine :: a
  }
  deriving (Show)

blankGrid :: Grid Cell
blankGrid = Grid Empty Empty Empty Empty Empty Empty Empty Empty Empty

instance Functor Grid where
  fmap f grid =
    Grid
      (f $ one grid)
      (f $ two grid)
      (f $ three grid)
      (f $ four grid)
      (f $ five grid)
      (f $ six grid)
      (f $ seven grid)
      (f $ eight grid)
      (f $ nine grid)

printGrid :: Show a => Grid a -> IO ()
printGrid grid =
  putStrLn ""
    *> putStrLn (" " ++ a ++ " | " ++ b ++ " | " ++ c ++ " ")
    *> putStrLn "---+---+---"
    *> putStrLn (" " ++ d ++ " | " ++ e ++ " | " ++ f ++ " ")
    *> putStrLn "---+---+---"
    *> putStrLn (" " ++ g ++ " | " ++ h ++ " | " ++ i ++ " ")
    *> putStrLn ""
  where
    grid' = show <$> grid
    a = one grid'
    b = two grid'
    c = three grid'
    d = four grid'
    e = five grid'
    f = six grid'
    g = seven grid'
    h = eight grid'
    i = nine grid'

updateGrid :: Grid Cell -> Cell -> String -> Grid Cell
updateGrid g cell cellNum =
  case cellNum of
    "1" -> Grid cell (two g) (three g) (four g) (five g) (six g) (seven g) (eight g) (nine g)
    "2" -> Grid (one g) cell (three g) (four g) (five g) (six g) (seven g) (eight g) (nine g)
    "3" -> Grid (one g) (two g) cell (four g) (five g) (six g) (seven g) (eight g) (nine g)
    "4" -> Grid (one g) (two g) (three g) cell (five g) (six g) (seven g) (eight g) (nine g)
    "5" -> Grid (one g) (two g) (three g) (four g) cell (six g) (seven g) (eight g) (nine g)
    "6" -> Grid (one g) (two g) (three g) (four g) (five g) cell (seven g) (eight g) (nine g)
    "7" -> Grid (one g) (two g) (three g) (four g) (five g) (six g) cell (eight g) (nine g)
    "8" -> Grid (one g) (two g) (three g) (four g) (five g) (six g) (seven g) cell (nine g)
    "9" -> Grid (one g) (two g) (three g) (four g) (five g) (six g) (seven g) (eight g) cell
