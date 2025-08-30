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

boardToScreen :: (Int, Int) -> (Float, Float)
boardToScreen (x, y) = (fromIntegral x * (fst cellRes), fromIntegral y * (snd cellRes))

cellColour :: Maybe Cell -> Color
cellColour Nothing = makeColorI 40 41 41 255
cellColour (Just cell) = makeColorI a b c 255
    where
        (a, b, c) = case cell of
            Green  -> (50,  168, 82)
            Yellow -> (168, 156, 50)
            Grey   -> (66,  60,  60)

-- TODO: make htis not an optional please
cellPicture :: Maybe Cell -> Maybe Int -> Picture
cellPicture _ Nothing = Blank
cellPicture cell (Just qpos) = translate (-w) (-h)
    $ translate qx qy
    $ color (cellColour cell)
    $ rectangleSolid w h
    where
        w = fst cellRes / 2
        h = snd cellRes / 2
        (qx, qy) = case qpos of
            1 -> (-w/2, h/2)
            2 -> (w/2, h/2)
            3 -> (-w/2, -h/2)
            4 -> (w/2, -h/2)
            _ -> (0, 0)

boardCells :: Board -> Picture
boardCells board = pictures $ map finalPicture $ assocs board
    where
        finalPicture (i, c) =
            (uncurry translate) (boardToScreen i) (cellPictures c)

        cellPictures Nothing = cellPicture Nothing Nothing
        cellPictures (Just (a, b, c, d)) = pictures $
            zipWith cellPicture
                (Just <$> [a, b, c, d])
                (Just <$> [1..])

boardGrid :: Board -> Picture
boardGrid _ = pictures $
    map
        (\x -> line [boardToScreen (x, 0), boardToScreen (x, snd boardSize)])
        [0..fst boardSize]
    ++ map
        (\y -> line [boardToScreen (0, y), boardToScreen (fst boardSize, y)])
        [0..snd boardSize]

boardChoosingPicture :: Board -> Picture
boardChoosingPicture board =
    pictures [ boardCells board
             , boardGrid board
             ]

boardFillingPicture :: Board -> Picture
boardFillingPicture _ = Blank

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (-sw/2) (-sh/2) $
    case gameState game of
        Choosing -> boardChoosingPicture (gameBoard game)
        Filling -> boardFillingPicture (gameBoard game)
    where
        sw = fromIntegral (fst screenRes)
        sh = fromIntegral (snd screenRes)
