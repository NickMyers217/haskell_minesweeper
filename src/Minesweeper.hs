{- |
 This module defines a Game of Minesweeper
-}
module Minesweeper where

import           Board
import           Cell

import           System.Random

-- | A Difficulty has a certain # of Bombs on the Board
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


-- | An Action can be a LeftClick or RightClick
data Action = LeftClick
            | RightClick
            deriving (Eq, Show)


-- | A Move is an Action, and a Pos to perform it at
type Move = (Action, Pos)


-- | A Game consists of the following:
-- | diff  = A Difficulty for the Board
-- | board = A Board
data Minesweeper = Game { diff  :: Difficulty
                        , board :: Board}
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


-- | Generate an infinite amount of random numbers
randomList :: (Num a, Random a) => a -> [a]
randomList limit = randomRs (0, limit) (mkStdGen 1337)


-- | Generate the correct number of Random Bombs on a Games Board
generateBombs :: Minesweeper -> Minesweeper
generateBombs Game { diff = d, board = b } = Game { diff  = d
                                                  , board = bombCells b randomPs
                                                  }
  where bombNum  = diffBombs d
        size     = diffSize d
        randomXs = take bombNum $ randomList (fst size - 1)
        randomYs = drop bombNum $ randomList (snd size - 1)
        randomPs = zip randomXs randomYs


-- | Calculate all the Nums for a Game
generateNums :: Minesweeper -> Minesweeper
generateNums g = Game { diff = diff g, board = calculateBoardNums $ board g }


-- | Return an empty Game for a given Difficulty
emptyGame :: Difficulty -> Minesweeper
emptyGame d = Game { diff = d, board = newBoard . diffSize $ d }


-- | Creates an empty Game to match a Difficulty
-- | Sets up the correct number of Random Bombs for the Game
-- | Then it calculates all the Nums for the Game
setupGame :: Difficulty -> Minesweeper
setupGame = generateNums . generateBombs . emptyGame


-- | Reveals the entire Games Board
revealGame :: Minesweeper -> Minesweeper
revealGame g = Game { diff = diff g, board = revealBoard $ board g }


-- | Uses an Action to advance the Game a turn
stepGame :: Minesweeper -> Move -> Minesweeper
stepGame g (a, p) = Game { diff = diff g, board = case a of
                           LeftClick  -> revealCell (board g) p
                           RightClick -> flagCell (board g) p
                         }


-- | Checks if a Game is lost
isLost :: Minesweeper -> Bool
isLost Game { board = b } = any isShown . filter isBomb . concat $ b


-- | Checks if a Game is won
isWon :: Minesweeper -> Bool
isWon g@(Game { board = b })
  | isLost g  = False
  | otherwise = all isShown . filter (not . isBomb) . concat $ b
