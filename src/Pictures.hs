module Pictures where

import Rules
import SideFun
import Data.List
import UIElements
import Components
import Debug.Trace
import BackgammonGame
import Graphics.Gloss

s :: Float
s = 0.85


drawDice :: Player -> Int -> Picture
drawDice _player id = wCubesRendered <> rCubesRendered
  where
      wCubesRendered = renderCubes _player (PawnWhite 0 False)
      rCubesRendered = renderCubes _player (PawnRed 0 False)

      renderCubes :: Player -> Pawn -> Picture
      renderCubes player_ testPawn = case (player_, testPawn) of
        (PlayerWhite, PawnWhite _ _) -> renderFocusedCubes testPawn
        (PlayerRed, PawnRed _ _)     -> renderFocusedCubes testPawn
        _                            -> justCubes testPawn

      renderFocusedCubes :: Pawn -> Picture
      renderFocusedCubes testPawn = case testPawn of
        (PawnWhite _ _) -> translate (3*cSize) 0 (fCube white) <> translate (5*cSize) 0 (fCube white)
        (PawnRed _ _)   -> translate (-3*cSize) 0 (fCube redChipColor) <> translate (-5*cSize) 0 (fCube redChipColor)

      justCubes :: Pawn -> Picture
      justCubes testPawn = case testPawn of
        (PawnWhite _ _) -> translate (3*cSize) 0 (cube white) <> translate (5*cSize) 0 (cube white)
        (PawnRed _ _)   -> translate (-3*cSize) 0 (cube redChipColor) <> translate (-5*cSize) 0 (cube redChipColor)

      cube :: Color -> Picture
      cube color_ = color color_ (rectangleSolid cSize cSize) 
                    <> translate (-cSize/5.3) (-cSize/4.3) (scale 0.1 0.1 (text (show id)))

      fCube :: Color -> Picture
      fCube color_ = scale 1.5 1.5 $ color focusColor (rectangleSolid (1.3*cSize) (1.3*cSize))
                     <> color color_ (rectangleSolid cSize cSize) 
                     <> translate (-cSize/5.3) (-cSize/4.3) (scale 0.1 0.1 (text (show id)))

      cSize = 0.7*pawnRadius


drawPawn :: Pawn -> Picture
drawPawn pawn = case pawn of
  (PawnRed _ focus)   -> redPawn focus
  (PawnWhite _ focus) -> whitePawn focus

  where redPawn focus  = translate pawnRadius (size*pawnRadius)
                         $ scale size size
                         $ pawnShape redChipColor pawnRadius focus
            where size = s * focusSize focus

        whitePawn focus = translate pawnRadius (size*pawnRadius)
                          $ scale size size
                          $ pawnShape whiteChipColor pawnRadius focus
            where size = s * focusSize focus

        pawnShape color_ rad focus
          | focus     = color focusColor (circleSolid (rad+5)) <> color color_ (circleSolid rad) :: Picture
          | otherwise = color color_ (circleSolid rad) :: Picture

        focusSize :: Bool -> Float
        focusSize focus
          | focus     = 1.2
          | otherwise = 1.0


drawTrack :: Track -> Color -> Picture
drawTrack points trackColor = case points of
  Nothing      -> trackShape
  (Just pawns) -> trackShape <> pictures (map translatePawn (sortBy sortGT pawns))

  where trackShape = color trackColor $ polygon [(0,0),(pWidth,0),(pWidth*0.5,pHeight)]

        translatePawn :: Pawn -> Picture
        translatePawn pawn = translate 0 (pos*2*pawnRadius*s) $ drawPawn pawn
          where pos = fromIntegral (pt pawn) :: Float

        sortGT pawn1 pawn2
          | isFocused pawn1 = GT
          | isFocused pawn2 = LT
          | otherwise = compare (pt pawn1) (pt pawn2)


drawBar :: Bar -> Picture
drawBar Nothing      = blank
drawBar (Just pawns) = pictures whitePictures <> pictures redPictures
      where
        whitePictures = map bChipRender ordWhites
        redPictures = map bChipRender ordReds

        bChipRender :: Pawn -> Picture
        bChipRender chip = scale 0.7 0.7
                           $ translate (-0.5*bWidth) h (drawPawn chip)
                 where h = case chip of
                             (PawnWhite _ _) -> -(fromIntegral screenHeight) - gap + 2*pawnRadius*fromIntegral (pt chip)
                             (PawnRed _ _)   ->  trace (show gap) (fromIntegral screenHeight) + gap - 1.5*pawnRadius - 2*pawnRadius*fromIntegral (pt chip)
                       gap = (-qHeight/2)-pawnRadius

        (whiteChips, redChips) = groupChips pawns
        (Just ordWhites, Just ordReds) = (organiseTrack (Just whiteChips), organiseTrack (Just redChips))

        groupChips :: [Pawn] -> ([Pawn], [Pawn])
        groupChips pawns_ = (whites, reds)
          where
              whites = filter whiteFilter pawns_
              reds = filter redFilter pawns_

        whiteFilter :: Pawn -> Bool
        whiteFilter pawn = case pawn of
          (PawnWhite _ _) -> True
          _               -> False

        redFilter :: Pawn -> Bool
        redFilter pawn = case pawn of
          (PawnRed _ _) -> True
          _             -> False


drawQuad :: [Track] -> Picture
drawQuad points
  = translate (-0.5*bWidth - qWidth) (-qHeight)
  $ translate (qWidth*0.5) (qHeight*0.5) (color quadColor (rectangleSolid qWidth qHeight))
  <> pictures (zipWith drawTractAt points [ 5-i | i <- [0..(-1+length points)]])


  where drawTractAt :: Track -> Int -> Picture
        drawTractAt point i = translate (fromIntegral i*pWidth) 0 $ drawTrack point trackColor
          where trackColor = if even i then evenTrackColor else oddTrackColor


drawBoard :: Game -> Picture
drawBoard (Game oldBoard _ player_ _) = pictures boardFrame

  where (bar, quads) = setPt oldBoard

        boardFrame = allQuads ++ [drawDice player_ 6, drawBar bar]
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


{-  
    TODOS
    * Add numbers at top and bottom of quads
    * Change color to something more pleasing
    * Pick and Place Pawn
    * Add boundary to chips
    * Casting of Die
    * Show possible Moves from a cast of Die
-}