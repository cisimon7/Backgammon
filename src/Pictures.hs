module Pictures where

import SideFun
import Components
import Debug.Trace
import Graphics.Gloss

s :: Float
s = 0.85

drawPawn :: Pawn -> Picture
drawPawn pawn = case pawn of
  (PawnRed _)   -> redPawn
  (PawnWhite _) -> whitePawn

  where redPawn   = translate pawnRadius (s*pawnRadius) $ scale s s $ color black pawnShape
        whitePawn = translate pawnRadius (s*pawnRadius) $ scale s s $ color white pawnShape

        pawnShape = circleSolid pawnRadius :: Picture


drawTrack :: Track -> Picture
drawTrack points = case points of
  Nothing      -> trackShape
  (Just pawns) -> trackShape <> pictures (map translatePawn pawns)

  where trackShape = color blue $ polygon [(0,0),(pWidth,0),(pWidth*0.5,pHeight)]

        translatePawn :: Pawn -> Picture
        translatePawn pawn = translate 0 (pos*2*pawnRadius*s) $ drawPawn pawn
          where pos = fromIntegral (pt pawn) :: Float


drawBar :: Picture
drawBar = color black $ rectangleSolid bWidth bHeight


drawQuad :: [Track] -> Picture
drawQuad points
  = translate (-0.5*bWidth - qWidth) (-qHeight)
  $ translate (qWidth*0.5) (qHeight*0.5) (color red $ rectangleSolid qWidth qHeight)
  <> pictures (zipWith drawTractAt points [ 5-i | i <- [0..(-1+length points)]])

  where drawTractAt :: Track -> Int -> Picture
        drawTractAt point i = translate (fromIntegral i*pWidth) 0 $ drawTrack point


drawBoard :: Board -> Picture
drawBoard (_, quads) = pictures
                       $ zipWith translateQuad [ q1, q2, q3, q4 ]
                             [ (qWidth+bWidth,0), (0,0), (0, qHeight), (qWidth+bWidth, qHeight) ]

  where translateQuad :: Picture -> (Float, Float) -> Picture
        translateQuad q (i,j) = translate i j q

        q1 = drawQuad $ map organise (quads !! 0)
        q2 = drawQuad $ map organise (quads !! 1)
        q3 = translate (-bWidth - qWidth) (-qHeight) $ rotate 180
             $ drawQuad $ map organise $ quads !! 2
        q4 = translate (-bWidth - qWidth) (-qHeight) $ rotate 180
             $ drawQuad $ map organise $ quads !! 3

        organise :: Track -> Track
        organise track = case track of
          Nothing -> Nothing
          Just pawns -> Just [ pawn { pt=i } | pawn <- pawns, i <- [0..(length pawns - 1)] ]


{- Takes two numbers of between 1..24 -}
pickPlacePawn :: Board -> (Int, Int) -> Pawn -> Board
pickPlacePawn (bar, oldBoard) (from, to) chip = (bar, newBoard)

  where newBoard = updateAt i (updateAt j (addPawn j chip)) lessBoard :: [Quad]

        lessBoard = updateAt x (updateAt y (removePawn k)) oldBoard :: [Quad]

        ((x, y), (i, j), k) = (getTrackId from, getTrackId to, pt chip)

        {- returns a tuple of quad position and track position -}
        getTrackId :: Int -> (Int, Int)
        getTrackId z = (floor (fromIntegral z /6), mod z 6)

        removePawn :: Int -> Track -> Track
        removePawn idx track = case track of
          Nothing -> Nothing
          (Just pawns)
            | null newPawns -> Nothing
            | otherwise     -> Just newPawns
            where newPawns = removeAt idx pawns

        addPawn :: Int -> Pawn -> Track -> Track
        addPawn idx pawn track = case track of
          Nothing -> Just [pawn]
          Just pawns -> Just $ updateAt idx (const pawn) pawns


{-  TODOS
    * Add numbers at top and bottom of quads
    * Change color to something more pleasing
    * Pick and Place Pawn
    * Add boundary to chips
-}

