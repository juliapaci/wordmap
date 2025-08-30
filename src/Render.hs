module Render where

import Graphics.Gloss
import Data.Array
import Game

screenRes :: (Int, Int)
screenRes = (400, 800)

cellRes :: (Float, Float)
cellRes = ( fromIntegral (fst screenRes)
          / fromIntegral (fst boardSize)
          , fromIntegral (snd screenRes)
          / fromIntegral (snd boardSize)
          )

backgroundColour :: Color
backgroundColour = makeColorI 201 189 183 255

window :: Display
window = InWindow "wordmap" screenRes (100, 100)

cellColour :: Maybe Cell -> Color
cellColour Nothing = makeColorI 20 20 20 255
cellColour (Just cell) = makeColorI a b c 255
    where
        (a, b, c) = case cell of
            Green  -> (50,  168, 82)
            Yellow -> (168, 156, 50)
            Grey   -> (21,  21,  28)

cellPicture :: Maybe Cell -> Picture
cellPicture cell = color (cellColour cell) $ rectangleSolid 10 10

boardCells :: Board -> Picture
boardCells board = pictures $
    map cellPictures $ map snd $ assocs board
    where
        cellPictures Nothing = cellPicture Nothing
        cellPictures (Just (a, b, c, d)) =
            pictures $ map cellPicture $ sequence (Just [a, b, c, d])

boardGrid :: Board -> Picture
boardGrid _ = translate (-sw/2) (-sh/2) $ pictures $
    map
        (\x -> line [(x * cw, 0), (x * cw, sh)])
        [0..fromIntegral (fst boardSize)]
    ++ map (\x -> line [(0, x * ch), (sw, x * ch)])
        [0..fromIntegral (snd boardSize)]
    where
        sw = fromIntegral (fst screenRes)
        sh = fromIntegral (snd screenRes)
        cw = fst cellRes
        ch = snd cellRes

boardChoosingPicture :: Board -> Picture
boardChoosingPicture board =
    pictures [ boardGrid board
             , boardCells board
             ]

boardFillingPicture :: Board -> Picture
boardFillingPicture _ = Blank

gameAsPicture :: Game -> Picture
gameAsPicture game =
    case gameState game of
        Choosing -> boardChoosingPicture (gameBoard game)
        Filling -> boardFillingPicture (gameBoard game)
