{- |
 This module defines Cells for the Minesweeper Board
-}
module Cell where

-- | A cell can be Hidden or Shown
data Visibility = Hidden
                | Shown
                deriving (Eq, Show)

-- | A Cell can be one of the following:
-- | A Number 1 through 8
-- | A Bomb
-- | Empty
-- | A Flag ontop of any of the previous
data Cell = Num Int Visibility
          | Bomb Visibility
          | Empty Visibility
          | Flag  Cell
          deriving (Eq, Show)

-- | Determine if a Cell is Shown
isShown :: Cell -> Bool
isShown c =
    case c of
      Num _ Shown -> True
      Bomb  Shown -> True
      Empty Shown -> True
      Flag  cell  -> isShown cell
      _           -> False

-- | Handles the flagging of cells:
-- | Flags a Hidden Num, Bomb, or Empty
-- | Unflags a Flag
-- | Does nothing to Shown Num, Bomb, or Empty
flagCell :: Cell -> Cell
flagCell c =
    case c of
      n@(Num _ Hidden) -> Flag n
      b@(Bomb Hidden)  -> Flag b
      e@(Empty Hidden) -> Flag e
      Flag cell        -> cell
      _                -> c

-- | Handles the revealing of cells:
-- | Reveals a Hidden Num, Bomb, or Empty
-- | Does nothing to a Flag
-- | Does nothing to a Shown Num, Bomb, or Empty
revealCell :: Cell -> Cell
revealCell c =
    case c of
      Num i Hidden -> Num i Shown
      Bomb Hidden  -> Bomb Shown
      Empty Hidden -> Empty Shown
      _            -> c

