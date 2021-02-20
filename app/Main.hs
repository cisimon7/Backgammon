module Main where

import Pictures
import Components
import Graphics.Gloss
--import Graphics.Gloss.Data.Color
--import Graphics.Gloss.Data.Display
--import Graphics.Gloss.Interface.Pure.Game (play)

window :: Display
window = InWindow "Backgammon Game" (screenWidth,screenHeight+40) (300,25)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = display window backgroundColor
            $ pictures [ drawBoard $ pickPlacePawn initBoard (1,23) (PawnWhite 1)
                       , drawBar ]

  where initBoard = (Nothing, allQuads) :: Board

        allQuads = [ q1, q2, q3, q4 ] :: [[Track]]

        q1 = [ n, Just [ PawnRed 0
                       , PawnRed 1
                       , PawnRed 2
                       , PawnRed 3
                       , PawnRed 4 ], n, n, n, n ]
        q2 = [ n, n, n, n, n, n ]
        q3 = [ n, n, n, n, n, n ]
        q4 = [ n, n, n, n, n, n ]

        n = Nothing


{-  TESTS
    * DrawBoard                                        -- passed
    * Pick and Place Checker from one track to another -- passed
-}