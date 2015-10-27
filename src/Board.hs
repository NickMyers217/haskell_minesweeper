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


-- | Change a Board's Cell by using a given function at a Pos
changeCell :: (Cell -> Cell) -> Board -> Pos -> Board
changeCell f b (x, y) = b !!= (y, row !!= (x, f cell))
    where row  = b   !! y
          cell = row !! x


-- | Turns a Cell into a Bomb on a Board at a given Pos
bombCell :: Board -> Pos -> Board
bombCell = changeCell makeBomb


-- | Turns a Cell into a Num on a Board at a given Pos
numCell :: Int -> Board -> Pos -> Board
numCell i = changeCell $ makeNum i


-- | Flags a Cell on a given Board at a given Pos
flagCell :: Board -> Pos -> Board
flagCell = changeCell flag


-- | Returns [Pos] representing the Cell's neighbors
getNeighbors :: Board -> Pos -> [Pos]
getNeighbors b (x,y)    = filter (`elem` boardIs) . map addVs $ vectors
    where vectors       = [ (-1,-1), (0,-1)
                          , (1,-1), (-1,0)
                          , (1,0), (-1,1)
                          , (0,1), (1,1) ]
          lazyIs        = [ [ (x,y) | x <- [0..] ] | y <- [0..] ]
          boardIs       = map snd . concat $ zipWith zip b lazyIs
          addVs (dx,dy) = (x + dx, y + dy)


-- | Reveal a Hidden Cell on a given Board at a given Pos
-- | Nums and Bombs are revealed normally
-- | Empty Cells need to recurse until they reach the closest Nums
-- | If this is passed a Shown Cell nothing should happen
revealCell :: Board -> Pos -> Board
revealCell b p
    | isShown c         = b
    | not . isEmpty $ c = revPos p
    | otherwise         = foldl revealCell (revPos p) (getNeighbors b p)
    where c      = getCell b p
          revPos = changeCell reveal b
