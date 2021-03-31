module Grid
  ( Grid,
    blankGrid,
    printGrid,
    getCell,
    putCell,
    detectWin,
    detectTie,
  )
where

import Cell

-- investigate Vector or containers as alternative to this?
--  a | b | c
-- ---+---+---
--  d | e | f
-- ---+---+---
--  g | h | i
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

toRows :: Grid a -> [(a, a, a)]
toRows (Grid a b c d e f g h i) = [row1, row2, row3, col1, col2, col3, aiDiagonal, cgDiagonal]
  where
    row1 = (a, b, c)
    row2 = (d, e, f)
    row3 = (g, h, i)
    col1 = (a, d, g)
    col2 = (b, e, h)
    col3 = (c, f, i)
    aiDiagonal = (a, e, i)
    cgDiagonal = (c, e, g)

printGrid :: Show a => Grid a -> IO ()
printGrid grid =
  putStrLn ""
    >> putStrLn (concat [" ", a, " | ", b, " | ", c, " "])
    >> putStrLn "---+---+---"
    >> putStrLn (concat [" ", d, " | ", e, " | ", f, " "])
    >> putStrLn "---+---+---"
    >> putStrLn (concat [" ", g, " | ", h, " | ", i, " "])
    >> putStrLn ""
  where
    Grid a b c d e f g h i = show <$> grid

getCell :: String -> Grid Cell -> Cell
getCell cellNum (Grid a b c d e f g h i) =
  case cellNum of
    "1" -> a
    "2" -> b
    "3" -> c
    "4" -> d
    "5" -> e
    "6" -> f
    "7" -> g
    "8" -> h
    "9" -> i

putCell :: String -> Cell -> Grid Cell -> Grid Cell
putCell cellNum x (Grid a b c d e f g h i) =
  case cellNum of
    "1" -> Grid x b c d e f g h i
    "2" -> Grid a x c d e f g h i
    "3" -> Grid a b x d e f g h i
    "4" -> Grid a b c x e f g h i
    "5" -> Grid a b c d x f g h i
    "6" -> Grid a b c d e x g h i
    "7" -> Grid a b c d e f x h i
    "8" -> Grid a b c d e f g x i
    "9" -> Grid a b c d e f g h x

detectWin :: Grid Cell -> Bool
detectWin = any checkThreeInARow . toRows

detectTie :: Grid Cell -> Bool
detectTie (Grid a b c d e f g h i) =
  Empty `notElem` [a, b, c, d, e, f, g, h, i]

-- private

checkThreeInARow :: (Cell, Cell, Cell) -> Bool
checkThreeInARow (a, b, c) = a /= Empty && a == b && a == c
