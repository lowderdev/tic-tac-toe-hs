module Lib
  ( run,
  )
where

import Cell
import Grid

run :: IO ()
run = runGame blankGrid X

runGame :: Grid Cell -> Cell -> IO ()
runGame grid cell = do
  printGrid grid
  printTurn cell
  printInstruction
  cellNum <- getLine
  handleMove grid cell cellNum

handleMove :: Grid Cell -> Cell -> String -> IO ()
handleMove grid cell cellNum =
  if validateMove grid cellNum
    then detectEndState grid cell cellNum
    else putStrLn "Invalid Move" >> runGame grid cell

detectEndState :: Grid Cell -> Cell -> String -> IO ()
detectEndState grid cell cellNum
  | detectWin nextGrid = putStrLn (show cell ++ " Wins!") >> printGrid nextGrid
  | detectTie nextGrid = putStrLn "Tie Game!" >> printGrid nextGrid
  | otherwise = runGame nextGrid (nextCell cell)
  where
    nextGrid = putCell cellNum cell grid

printTurn :: Cell -> IO ()
printTurn cell = putStrLn $ show cell ++ "'s turn"

printInstruction :: IO ()
printInstruction = putStrLn "Enter number 1-9" *> putStrLn ""

validateMove :: Grid Cell -> String -> Bool
validateMove grid cellNum = invalidCellNum && cellTaken
  where
    invalidCellNum = cellNum `notElem` map show [1 .. 9]
    cellTaken = getCell cellNum grid /= Empty

nextCell :: Cell -> Cell
nextCell X = O
nextCell O = X
