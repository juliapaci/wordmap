module Main (main) where

import Lib
import Graphics.Gloss

import Game
import Render

transformGame _ game = game

main :: IO ()
main = play
    window
    backgroundColour
    30
    initialGame
    gameAsPicture
    transformGame
    (const id)
