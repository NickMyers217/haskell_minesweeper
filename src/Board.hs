{- |
 This module defines the Minesweeper Board
-}
module Board where

import Cell

-- | A Size is the (cols, rows) for a board
type Size = (Int, Int)

-- | A Board is a grid of Maybe Cells
type Board = [[Maybe Cell]]

-- | Creates a new Board of a given Size
newBoard :: Size -> Board
newBoard (cols,rows) = [ [ Nothing | c <- [1..cols] ]
                                   | r <- [1..rows] ]
