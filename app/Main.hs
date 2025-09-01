module Main (main) where

import Graphics.Gloss

import Game
import Render
import Logic

main :: IO ()
main = play
    window
    backgroundColour
    30
    initialGame
    gameAsPicture
    transformGame
    (const id)
