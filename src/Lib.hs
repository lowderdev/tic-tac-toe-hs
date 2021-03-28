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
  printInstruction
  move <- getLine
  case validateMove grid move of
    Left result -> putStrLn result >> runGame grid cell
    Right result ->
      let newGrid = updateGrid grid cell move
          go (Just cell) = putStrLn (show cell ++ " Wins!") >> printGrid newGrid
          go Nothing =
            if detectTie newGrid
              then putStrLn "Tie Game!" >> printGrid newGrid
              else runGame newGrid (nextCell cell)
       in go $ detectWin newGrid

printInstruction :: IO ()
printInstruction = putStrLn "Enter number 1-9" *> putStrLn ""

validateMove :: Grid Cell -> String -> Either String String
validateMove grid move
  | move `notElem` map show [1 .. 9] = invalidMoveError
  | getCell move grid /= Empty = invalidMoveError
  | otherwise = Right move
  where
    invalidMoveError = Left "Invalid move, please enter number 1-9"

nextCell :: Cell -> Cell
nextCell X = O
nextCell O = X
