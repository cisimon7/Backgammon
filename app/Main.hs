module Main where

import Effects
import Pictures
import UIElements
import Components
import Graphics.Gloss
import BackgammonGame
import Graphics.Gloss.Interface.Pure.Game (play, Event)

window :: Display
window = InWindow "Backgammon Game" (screenWidth,screenHeight+40) (300,25)


main :: IO ()
main = play window backgroundColor 30 backgammonGame
                                      drawBoard 
                                      transformGame 
                                      (const id)
                
                

{-  TESTS
    * DrawBoard                                        -- passed
    * Pick and Place Checker from one track to another -- passed
-}