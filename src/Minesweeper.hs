{- |
 This module defines a Game of Minesweeper
-}
module Minesweeper where

import           Board

-- | A difficulty has a certain # of bombs on the board
type BombCount = Int


-- | A Difficulty for a Game can be one of the following:
-- | Easy   = A 10 by 10 board
-- | Medium = A 15 by 15 board
-- | Hard   = 20 by 20
-- | Insane = 30 by 30
-- | Custom = A user specified Size
data Difficulty = Easy
                | Medium
                | Hard
                | Insane
                | Custom Size BombCount
                deriving (Eq, Show)


-- | A Game consists of the following:
-- | diff  = A Difficulty for the Board
-- | board = A Board
data Minesweeper = Game { diff  :: Difficulty
                        , board :: Board
                        }
                        deriving (Show)


-- | Return the Size for a given Difficulty
diffSize :: Difficulty -> Size
diffSize d = case d of
  Easy       -> (10, 10)
  Medium     -> (15, 15)
  Hard       -> (20, 20)
  Insane     -> (30, 30)
  Custom s _ -> s


-- | Return the number of Bombs for a Difficulty
diffBombs :: Difficulty -> BombCount
diffBombs d = case d of
  Easy       -> 10
  Medium     -> 20
  Hard       -> 30
  Insane     -> 40
  Custom _ c -> c


-- | TODO: Make it generate random bombs and shit
-- | Return an empty Game for a given Difficulty
emptyGame :: Difficulty -> Minesweeper
emptyGame d = Game { diff = d, board = newBoard . diffSize $ d }
