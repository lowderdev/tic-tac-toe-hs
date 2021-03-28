module Grid where

import Cell

-- investigate Vector or containers? as alternative to this

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

getCell :: String -> Grid Cell -> Cell
getCell cellNum grid =
  case cellNum of
    "1" -> one grid
    "2" -> two grid
    "3" -> three grid
    "4" -> four grid
    "5" -> five grid
    "6" -> six grid
    "7" -> seven grid
    "8" -> eight grid
    "9" -> nine grid

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

detectTie :: Grid Cell -> Bool
detectTie (Grid a b c d e f g h i) =
  Empty `notElem` [a, b, c, d, e, f, g, h, i]

-- Refactor to the following?
-- detectWin (Grid a b c d e f g h i)
--   | a /= Empty && a == b && a == c = Just a
--   | d /= Empty -- and so on
detectWin :: Grid Cell -> Maybe Cell
detectWin grid =
  case grid of
    Grid X X X _ _ _ _ _ _ -> Just X
    Grid _ _ _ X X X _ _ _ -> Just X
    Grid _ _ _ _ _ _ X X X -> Just X
    Grid X _ _ X _ _ X _ _ -> Just X
    Grid _ X _ _ X _ _ X _ -> Just X
    Grid _ _ X _ _ X _ _ X -> Just X
    Grid X _ _ _ X _ _ _ X -> Just X
    Grid _ _ X _ X _ X _ _ -> Just X
    Grid O O O _ _ _ _ _ _ -> Just O
    Grid _ _ _ O O O _ _ _ -> Just O
    Grid _ _ _ _ _ _ O O O -> Just O
    Grid O _ _ O _ _ O _ _ -> Just O
    Grid _ O _ _ O _ _ O _ -> Just O
    Grid _ _ O _ _ O _ _ O -> Just O
    Grid O _ _ _ O _ _ _ O -> Just O
    Grid _ _ O _ O _ O _ _ -> Just O
    _ -> Nothing
