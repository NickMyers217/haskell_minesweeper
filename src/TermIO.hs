{-|
 This module handles the following:
 - Reading test Boards from files
 - Displaying Games in the terminal
-}
module TermIO where

import           Board
import           Cell
import           Minesweeper

import           Data.Char


-- | Turns a Cell into a String
showCell :: Cell -> Char
showCell c =
    case c of
      Num i Shown -> intToDigit i
      Bomb  Shown -> 'B'
      Empty Shown -> 'o'
      Flag  _     -> 'F'
      _           -> '#'


-- | Turns a Board into a String
showBoard :: Board -> String
showBoard = unlines . map (map showCell)


-- | Prints a Board to the terminal
printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


-- | Prints a Game to the terminal
printGame :: Minesweeper -> IO ()
printGame = printBoard . board


-- | Gets the player's chosen Pos
getPos :: IO Pos
getPos = do
  putStr "Enter a x coordinate: "
  x <- getLine
  putStr "Enter a y coordniate: "
  y <- getLine
  return (read x :: Int, read y :: Int)


-- | Gets the player's chosen Action
getAction :: IO Action
getAction = do
  putStr "(Left) or (Right) click: "
  actStr <- getLine
  case actStr of
    "Left"  -> return LeftClick
    "Right" -> return RightClick
    _       -> getAction


-- | Gets the players Move
getMove :: IO Move
getMove = do
  act <- getAction
  pos <- getPos
  return (act, pos)


-- | Plays a Game
playTermGame :: Minesweeper -> IO ()
playTermGame g = do
  printGame g
  if isLost g
    then putStrLn "You Lost!"
    else if isWon g
         then putStrLn "You Won!"
         else do
              move <- getMove
              playTermGame $ stepGame g move
