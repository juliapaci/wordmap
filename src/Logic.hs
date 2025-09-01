module Logic where

import Debug.Trace

import Game
import Render
import qualified Render (screenToBoard)
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

isCellValid = inRange ((0, 0), ((fst boardSize) - 1, (snd boardSize) - 1))

clickCell game spos
    | isCellValid bpos =
        game { gameBoard = board // [(bpos, nextCells subCell (board ! bpos))]}
    | otherwise = game
    where
        board = gameBoard game

        bpos = screenToBoard spos
        subCell = case (scx, scy) of
            (0, 0) -> 2
            (0, 1) -> 0
            (1, 0) -> 3
            (1, 1) -> 1
            _ -> traceShow (scx, scy) 0

        (scx, scy) = ( round ((sx - bx) / cw)
                     , round ((sy - by) / ch)
                     )
            where
                (cw, ch) = cellRes
                (sx, sy) = screenWithOrigin spos
                (bx, by) = boardToScreen bpos

        nextCells q (a, b, c, d) = replaceAt q cells $ nextColour $ cells !! q
            where
                cells = [a, b, c, d]
                replaceAt q l r = (ra, rb, rc, rd)
                    where
                        (x,_:xs) = splitAt q $ l ++ [Empty]
                        [ra, rb, rc, rd, _] = x ++ [r] ++ xs

        nextColour cell = case cell of
            Green  -> Yellow
            Yellow -> Grey
            Grey   -> Empty
            Empty  -> Green

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
        Choosing -> clickCell game mousePos
        Filling  -> game
transformGame _ game = game
