module Game where

import Data.Array

data Cell = Green | Yellow | Grey deriving (Eq, Show)
type Board = Array (Int, Int) (Maybe (Cell, Cell, Cell, Cell))

data State = Choosing | Filling deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , gameState :: State
                 } deriving (Eq, Show)

boardSize :: (Int, Int)
boardSize = (5, 6)

initialGame :: Game
initialGame = Game { gameBoard = array size $ zip (range size) (cycle [Just (Grey, Yellow, Green, Grey)])
                   , gameState = Choosing
                   }
    where size = ((0, 0), boardSize)
