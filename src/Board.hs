{- |
 This module defines the Minesweeper Board
-}
module Board where

import Cell

-- | A Size is the (cols, rows) for a board
type Size  = (Int, Int)

-- | A Point is an (x, y) coordinate
type Pos   = (Int, Int)

-- | A Board is a grid of Cells
type Board = [[Cell]]


-- | Creates a new Board of a given Size
newBoard :: Size -> Board
newBoard (cols,rows) = [ [ Empty Hidden | c <- [1..cols] ]
                                        | r <- [1..rows] ]

-- | Get a Cell from a Board at a given Pos
getCell :: Board -> Pos -> Cell
getCell b (x,y) = b !! y !! x


-- | Update a list with a new value at a given index
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i,v) 
    | i > -1 && i < length xs = take i xs ++ [v] ++ drop (i + 1) xs
    | otherwise               = error "Can't modify the list at the given index."


-- | Change a Board's Cell by using a given function at that Pos
changeCell :: Board -> (Cell -> Cell) -> Pos -> Board
changeCell b f (x, y) = b !!= (y, row !!= (x, f cell))
    where row  = b !! y
          cell = row !! x


-- | Flags a cell on the board
flagBoardCell :: Board -> Pos -> Board
flagBoardCell b = changeCell b flagCell
