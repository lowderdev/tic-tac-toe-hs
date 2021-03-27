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
  either left right $ validateMove grid move
  where
    left = \s -> putStrLn s >> runGame grid cell
    right = \s -> runGame (updateGrid grid cell s) (nextCell cell)

printInstruction :: IO ()
printInstruction = putStrLn "Enter number 1-9" *> putStrLn ""

validateMove :: Grid Cell -> String -> Either String String
validateMove grid str = if str `elem` map show [1 .. 9] then Right str else Left "Invalid move, please enter number 1-9"

nextCell :: Cell -> Cell
nextCell X = O
nextCell O = X
