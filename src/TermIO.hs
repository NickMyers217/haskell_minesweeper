{-|
 This module handles the following:
 - Reading test Boards from files
 - Displaying Games in the terminal
-}
module TermIO where

import Cell
import Board
import Minesweeper

import Data.Char

showCell :: Cell -> Char
showCell c =
    case c of
      Num i Shown -> intToDigit i
      Bomb  Shown -> 'B'
      Empty Shown -> 'E'
      Flag  _     -> 'F'
      _           -> '#'

showBoard :: Board -> String
showBoard = unlines . map (map showCell)

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard
