module Main where

import Pictures
import Components
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game (play)

window :: Display
window = InWindow "Backgammon Game" (screenWidth,screenHeight+40) (50,50)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = display window backgroundColor 
            $ pictures [ renderBoard
                       , renderBar
                       , checker     ]
