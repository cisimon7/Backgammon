module Pictures where

import SideFun
import Components
import Debug.Trace
import Graphics.Gloss
import Data.List

s :: Float
s = 0.85

drawPawn :: Pawn -> Picture
drawPawn pawn = case pawn of
  (PawnRed _ focus)   -> redPawn focus
  (PawnWhite _ focus) -> whitePawn focus

  where redPawn   focus = translate pawnRadius (size*pawnRadius) $ scale size size $ pawnShape black pawnRadius focus
            where size = s * focusSize focus
        whitePawn focus = translate pawnRadius (size*pawnRadius) $ scale size size $ pawnShape white pawnRadius focus
            where size = s * focusSize focus

        pawnShape color_ rad focus
          | focus     = color green (circleSolid (rad+5)) <> color color_ (circleSolid rad) :: Picture
          | otherwise = color color_ (circleSolid rad) :: Picture

        focusSize :: Bool -> Float
        focusSize focus
          | focus     = 1.2
          | otherwise = 1.0


drawTrack :: Track -> Picture
drawTrack points = case points of
  Nothing      -> trackShape
  (Just pawns) -> trackShape <> pictures (map translatePawn (sortBy sortGT pawns))

  where trackShape = color blue $ polygon [(0,0),(pWidth,0),(pWidth*0.5,pHeight)]

        translatePawn :: Pawn -> Picture
        translatePawn pawn = translate 0 (pos*2*pawnRadius*s) $ drawPawn pawn
          where pos = fromIntegral (pt pawn) :: Float

        sortGT pawn1 pawn2
          | isFocused pawn1 = GT
          | isFocused pawn2 = LT
          | otherwise = compare (pt pawn1) (pt pawn2)


drawBar :: Picture
drawBar = color black $ rectangleSolid bWidth bHeight


organiseTrack :: Track -> Track
organiseTrack track = case track of
  Nothing -> Nothing
  Just pawns -> Just [ pawn { pt=i } | (pawn, i) <- zip pawns [0..(length pawns - 1)] ]


drawQuad :: [Track] -> Picture
drawQuad points
  = translate (-0.5*bWidth - qWidth) (-qHeight)
  $ translate (qWidth*0.5) (qHeight*0.5) (color red (rectangleSolid qWidth qHeight))
  <> pictures (zipWith drawTractAt points [ 5-i | i <- [0..(-1+length points)]])


  where drawTractAt :: Track -> Int -> Picture
        drawTractAt point i = translate (fromIntegral i*pWidth) 0 $ drawTrack point


drawBoard :: Board -> Picture
drawBoard (_, quads) = pictures boardFrame

  where boardFrame = drawBar : allQuads
        allQuads = zipWith translateQuad [ q1, q2, q3, q4 ]
                                         [ (qWidth+bWidth,0), (0,0), (0, qHeight), (qWidth+bWidth, qHeight) ]

        {- Function to place each quad at it's location -}
        translateQuad :: Picture -> (Float, Float) -> Picture
        translateQuad q (i,j) = translate i j q

        {- Converting Each Quad to a picture -}
        q1 = drawQuad (head quads)
        q2 = drawQuad (quads !! 1)
        q3 = translate (-bWidth - qWidth) (-qHeight) $ rotate 180 $ drawQuad (quads !! 2)
        q4 = translate (-bWidth - qWidth) (-qHeight) $ rotate 180 $ drawQuad (quads !! 3)


{- Takes two numbers of between 1..24 and a pawn.
   Function removes the given pawn at an index and places it at a new index -}
pickPlacePawn :: Board -> (Int, Int) -> Pawn -> Board
pickPlacePawn (bar, oldBoard) (from, to) chip = case oldBoard !! x !! y of
    {- If Track is empty, return oldBoard, else pick and place --}
    Nothing  -> (bar, oldBoard)
    (Just _) -> (bar, newBoard)

  where
        {- Returns the board with the removed pawn now placed at new location -}
        newBoard = updateAt i (updateAt j (addPawn j chip)) lessBoard :: [Quad]

        {- Returns a new board with the given pawn removed from the board -}
        lessBoard = updateAt x (updateAt y (removePawn k)) oldBoard :: [Quad]

        {- Gets the from and to quad index, track index and chip height -}
        ((x, y), (i, j), k) = (getTrackId from, getTrackId to, pt chip)

        {- returns a tuple of quad position and track position -}
        getTrackId :: Int -> (Int, Int)
        getTrackId z = (floor (fromIntegral z /6), mod z 6)

        {- Removes a pawn at a specific index -}
        removePawn :: Int -> Track -> Track
        removePawn idx track = case track of
          Nothing -> Nothing
          (Just pawns)
            | null newPawns -> Nothing
            | otherwise     -> organiseTrack $ Just newPawns
            where newPawns = removeAt idx pawns

        {- Adds a pawn at a given pt -}
        addPawn :: Int -> Pawn -> Track -> Track
        addPawn _ pawn track = case track of
          Nothing -> Just [pawn { pt=0 }]
          Just _  -> newTrack
            where 
                l = length track
                newTrack = case organiseTrack track of
                    Nothing -> Nothing
                    (Just orderedPawns) -> Just (orderedPawns ++ [pawn { pt=l }])


{-  
    TODOS
    * Add numbers at top and bottom of quads
    * Change color to something more pleasing
    * Pick and Place Pawn
    * Add boundary to chips
-}