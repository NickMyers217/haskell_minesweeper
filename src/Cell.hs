{- |
 This module defines Cells for the Minesweeper Board
-}
module Cell where

-- | A Point is an (x, y) coordinate
type Pos  = (Int, Int)

-- | A Cell can be one of the following:
-- | A Number 1 through 8
-- | A Bomb
-- | A Flag ontop of either a Number or Bomb
data Cell = Num Int
          | Bomb
          | Flag Cell
          deriving (Eq, Show)
