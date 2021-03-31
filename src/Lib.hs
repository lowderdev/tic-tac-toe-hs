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
  printMovePrompt
  cellNum <- getLine
  updateGrid grid cell $ validateMove grid cell cellNum

printTurn :: Cell -> IO ()
printTurn cell = putStrLn $ show cell ++ "'s turn"

printMovePrompt :: IO ()
printMovePrompt = putStr "Enter number 1-9: "

validateMove :: Grid Cell -> Cell -> String -> Either String String
validateMove grid cell cellNum
  | invalidCellNum = Left "Move is not a num 1-9"
  | cellTaken = Left "Cell is already taken"
  | otherwise = Right cellNum
  where
    invalidCellNum = cellNum `notElem` map show [1 .. 9]
    cellTaken = getCell cellNum grid /= Empty

updateGrid :: Grid Cell -> Cell -> Either String String -> IO ()
updateGrid grid cell (Left error) = putStrLn error >> runGame grid cell
updateGrid grid cell (Right cellNum)
  | detectWin nextGrid = printGrid nextGrid >> printWinMessage cell
  | detectTie nextGrid = printGrid nextGrid >> printTieMessage
  | otherwise = runGame nextGrid (nextCell cell)
  where
    nextGrid = putCell cellNum cell grid

printWinMessage :: Cell -> IO ()
printWinMessage cell = putStrLn $ concat ["===== ", show cell, " Wins!", " ====="]

printTieMessage :: IO ()
printTieMessage = putStrLn "===== Tie Game! ====="
