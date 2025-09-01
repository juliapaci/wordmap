{-# language
    DataKinds
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , TypeApplications
  , TypeOperators
  , ViewPatterns
  #-}

module Render where

import Graphics.Gloss
import Data.Array
import Data.Either
import Refined.Unsafe
import Refined
import Game


screenRes :: (Int, Int)
screenRes = (400, 800)
origin = ( -(fromIntegral (fst screenRes)) / 2
         , -(fromIntegral (snd screenRes)) / 2
         )
screenWithOrigin (x, y) = (x - (fst origin), y - (snd origin))

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
screenToBoard :: (Float, Float) -> (Int, Int)
-- TODO: plsplspls just make a `(a -> b) -> (a, a) -> (b, b)` :pray
screenToBoard spos =
        ( floor $ x / fst cellRes
        , floor $ y / snd cellRes
        )
        where (x, y) = screenWithOrigin spos

cellColour :: Cell -> Color
cellColour cell = makeColorI a b c 255
    where
        (a, b, c) = case cell of
            Green  -> (50,  168, 82)
            Yellow -> (168, 156, 50)
            Grey   -> (66,  60,  60)
            Empty  -> (40,  41,  41)

type Quadrant = Refined (GreaterThan 0 && LessThan 5) Int
cellPicture :: Cell -> Quadrant -> Picture
cellPicture cell q
    = translate (-w) (-h)
    $ translate qx qy
    $ color (cellColour cell)
    $ rectangleSolid w h
    where
        w = fst cellRes / 2
        h = snd cellRes / 2
        (qx, qy) = case unrefine q of
            1 -> (-w/2, h/2)
            2 -> (w/2, h/2)
            3 -> (-w/2, -h/2)
            4 -> (w/2, -h/2)

boardCells :: Board -> Picture
boardCells board = pictures $ map finalPicture $ assocs board
    where
        finalPicture (i, c) =
            (uncurry translate) (boardToScreen i) (cellPictures c)

        cellPictures (a, b, c, d) = pictures $
            zipWith cellPicture [a, b, c, d] $ unsafeRefine <$> [1..]

boardGrid :: Board -> Picture
boardGrid _ = pictures $
    map
        (\x -> line $ map boardToScreen [(x, 0), (x, snd boardSize)])
        [0..fst boardSize]
    ++ map
        (\y -> line $ map boardToScreen [(0, y), (fst boardSize, y)])
        [0..snd boardSize]

boardChoosingPicture :: Board -> Picture
boardChoosingPicture board =
    pictures [ boardCells board
             , boardGrid board
             ]

boardFillingPicture :: Board -> Picture
boardFillingPicture _ = Blank

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fst origin) (snd origin) $
    case gameState game of
        Choosing -> boardChoosingPicture (gameBoard game)
        Filling -> boardFillingPicture (gameBoard game)
