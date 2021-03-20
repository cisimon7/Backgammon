module Main where

import Effects
import Pictures
import Components
import Graphics.Gloss
--import Graphics.Gloss.Data.Color
--import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game (play, Event)

window :: Display
window = InWindow "Backgammon Game" (screenWidth,screenHeight+40) (300,25)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

setPt :: Board -> Board
setPt (bar, quads) = (bar, map (map organiseTrack) quads) 


main :: IO ()
main = play window backgroundColor 30 (setPt $ pickPlacePawn initBoard (1,18) (PawnWhite 2 False)) 
                                      (drawBoard . setPt) 
                                      transformGame 
                                      (const id)
            

  where initBoard = (Nothing, allQuads) :: Board

        allQuads = [ q1, q2, q3, q4 ] :: [[Track]]

        q1 = [ n, set1, n, n, n, n ]
        q2 = [ n, n, n, n, n, n ]
        q3 = [ n, n, n, set2, n, n ]
        q4 = [ n, n, n, n, n, n ]

        n    = Nothing
        set1 = Just [ PawnWhite   0 False
                    , PawnWhite   1 False
                    , PawnWhite   2 False
                    , PawnWhite   3 False
                    , PawnWhite   4 False ]
        
        set2 = Just [ PawnRed    0 False
                    , PawnRed    1 False
                    , PawnRed    2 False ]
       
        
        
--        initRender = pictures [ drawBoard initBoard -- $ pickPlacePawn initBoard (1,13) (PawnWhite 2 False)
--                                      , drawBar ]
                
                
        


{-  TESTS
    * DrawBoard                                        -- passed
    * Pick and Place Checker from one track to another -- passed
-}