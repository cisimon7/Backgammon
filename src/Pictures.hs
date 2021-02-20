module Pictures where

import Components
import Graphics.Gloss

checker :: Picture
checker = color green $ circleSolid chkRad

s :: Float
s = 0.8

redChecker, whiteChecker :: Picture
redChecker   = translate chkRad (s*chkRad) $ scale s s $ color red checker
whiteChecker = translate chkRad (s*chkRad) $ scale s s $ color white checker

renderPoint :: Point_ -> Picture
renderPoint points = case points of
  Nothing         -> triangle
  (Just checkers) -> triangle <> pictures (map (\chkr -> placeChecker chkr) checkers)
  
  where triangle = color blue $ polygon [(0,0),(pWidth,0),(pWidth*0.5,pHeight)] 
        
        placeChecker :: Checker -> Picture
        placeChecker checker' = case checker' of
            (CheckerRed y)   -> translate 0 (y*2*chkRad*s) redChecker
            (CheckerWhite y) -> translate 0 (y*2*chkRad*s) whiteChecker

renderBar :: Picture
renderBar = color black $ rectangleSolid bWidth bHeight

renderQuad :: [Point_] -> Picture
renderQuad points 
  = translate (-0.5*bWidth - qWidth) (-qHeight) 
  $ (translate (qWidth*0.5) (qHeight*0.5) $ color red $ rectangleSolid qWidth qHeight)
  <> (pictures $ map drawPtAt (zip points [ i | i <- [0..(-1+length points)]]))
  where drawPtAt :: (Point_, Int) -> Picture
        drawPtAt (point, i) = translate (fromIntegral i*pWidth) 0 $ renderPoint point

renderBoard :: Picture
renderBoard = pictures (map drawQuad $ zip [ q1, q2, q3, q4 ] 
                                           [ (0,0), (qWidth+bWidth,0), (qWidth+bWidth, qHeight), (0, qHeight) ]) 
  where drawQuad :: (Picture, (Float, Float)) -> Picture
        drawQuad (q, (i,j)) = translate i j q 
        
        q1 = renderQuad [ Nothing, Nothing, Nothing, Nothing, Nothing, Just [CheckerRed 0, CheckerRed 1, CheckerRed 2, CheckerRed 3, CheckerRed 4] ]
        q2 = q1
        q3 = translate (-bWidth - qWidth) (-qHeight) $ rotate 180 $ renderQuad [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        q4 = q3
        