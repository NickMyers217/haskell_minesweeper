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

-- | Determine if a Cell is Empty
isEmpty :: Cell -> Bool
isEmpty c =
    case c of
      Empty _   -> True
      Flag cell -> isEmpty cell
      _         -> False

-- | Determine if a Cell is a Bomb
isBomb :: Cell -> Bool
isBomb c =
    case c of
      Bomb _    -> True
      Flag cell -> isBomb cell
      _         -> False

-- | Determine if a cell is a Num
isNum :: Cell -> Bool
isNum c =
    case c of
      Num _ _   -> True
      Flag cell -> isNum cell
      _         -> False

-- | Make a Cell a Bomb
makeBomb :: Cell -> Cell
makeBomb c
    | isShown c = Bomb Shown
    | otherwise = Bomb Hidden

-- | Make a Cell a Num
makeNum :: Int -> Cell -> Cell
makeNum i c
    | isShown c = Num i Shown
    | otherwise = Num i Hidden

-- | Handles the flagging of cells:
-- | Flags a Hidden Num, Bomb, or Empty
-- | Unflags a Flag
-- | Does nothing to Shown Num, Bomb, or Empty
flag :: Cell -> Cell
flag c =
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
reveal :: Cell -> Cell
reveal c =
    case c of
      Num i Hidden -> Num i Shown
      Bomb Hidden  -> Bomb Shown
      Empty Hidden -> Empty Shown
      _            -> c
