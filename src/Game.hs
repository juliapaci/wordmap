module Game where

import Data.Array

data Cell = Green | Yellow | Grey | Empty deriving (Eq, Show)
type Square t = (t, t, t, t)
type Cells = Square Cell
type Board = Array (Int, Int) Cells

data State = Choosing | Filling deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , gameState :: State
                 } deriving (Eq, Show)

boardSize :: (Int, Int)
boardSize = (5, 6)

initialGame :: Game
initialGame = Game { gameBoard = array size $ zip (range size) (cycle [(Empty, Empty, Empty, Empty)])
                   , gameState = Choosing
                   }
    where size = ((0, 0), ((fst boardSize) - 1, (snd boardSize) - 1))
