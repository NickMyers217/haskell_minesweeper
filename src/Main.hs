{- |
 This Module handles playing Games of Minesweeper
-}
module Main where


import           Minesweeper
import           TermIO


-- | The main function
main :: IO ()
main = playTermGame $ setupGame Medium
