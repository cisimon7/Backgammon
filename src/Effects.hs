{-# LANGUAGE BlockArguments #-}

module Effects where

import SideFun
import Components
import Debug.Trace
import System.Random
import BackgammonGame
import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game (Event)
import GHC.IO.Unsafe (unsafePerformIO)


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


resetAllFocus :: Board -> Board
resetAllFocus (bar, quads) = (bar, map (map resetFocus) quads)
  where
      resetFocus :: Track -> Track
      resetFocus Nothing = Nothing
      resetFocus (Just pawns) = Just $ map reset pawns

      reset :: Pawn -> Pawn
      reset pawn = pawn { isFocused=False }


chipIdx :: (Float, Float) -> Maybe (Int, Int, Int)
chipIdx pos = getIdx (quadIdx pos) (trackIdx pos) (yIdx pos)

  where getIdx :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe (Int, Int, Int)
        getIdx Nothing _ _ = Nothing
        getIdx _ Nothing _ = Nothing
        getIdx _ _ Nothing = Nothing
        getIdx (Just x) (Just y) (Just z) = Just (x, y, z)

        quadIdx :: (Float, Float) -> Maybe Int
        quadIdx (x,y)
          | x>(bWidth/2) && x<(qWidth+(bWidth/2)) && y>0 && y<qHeight        = Just 3
          | x<(-(bWidth/2)) && x>(-qWidth-(bWidth/2)) && y>0 && y<qHeight    = Just 2
          | x<(-(bWidth/2)) && x>(-qWidth-(bWidth/2)) && y>(-qHeight) && y<0 = Just 1
          | x>(bWidth/2) && x<(qWidth+(bWidth/2)) && y>(-qHeight) && y<0     = Just 0
          | otherwise = Nothing

        trackIdx :: (Float, Float) -> Maybe Int
        trackIdx (x,y)
          | x>(bWidth/2) && x<(qWidth+(bWidth/2)) && y>0 && y<qHeight        = Just $ floor ((x-(bWidth/2))/pWidth)
          | x<(-(bWidth/2)) && x>(-qWidth-(bWidth/2)) && y>0 && y<qHeight    = Just $ 5 - floor (abs(x+(bWidth/2))/pWidth)
          | x<(-(bWidth/2)) && x>(-qWidth-(bWidth/2)) && y>(-qHeight) && y<0 = Just $ floor (abs(x+(bWidth/2))/pWidth)
          | x>(bWidth/2) && x<(qWidth+(bWidth/2)) && y>(-qHeight) && y<0     = Just $ 5 - floor ((x-(bWidth/2))/pWidth)
          | otherwise                                                        = Nothing

        yIdx :: (Float, Float) -> Maybe Int
        yIdx (_,y)
          | y>0 && y<qHeight    = Just $ 5 - floor (y/(s*pWidth))
          | y>(-qHeight) && y<0 = Just $ abs (floor (abs y/(s*pWidth)) - 5)
          | otherwise           = Nothing


getFocusedChip :: [Quad] -> Maybe (Int, Pawn)
getFocusedChip quads = getFocus $ zipWith combine (concatMap (map check) quads) allIdx
  where
      allIdx = [ tIdx_+(6*qIdx_)-1 | qIdx_ <- [0..3], tIdx_ <- [1..6] ]

      combine :: Maybe Pawn -> Int -> Maybe (Int, Pawn)
      combine Nothing _ = Nothing
      combine (Just pawn) idx = Just (idx, pawn)


getFocus :: [Maybe (Int, Pawn)] -> Maybe (Int, Pawn)
getFocus list = do
  let first = find isJust list
  case first of
    Nothing             -> Nothing
    Just Nothing        -> Nothing
    Just (Just content) -> Just content


check :: Track -> Maybe Pawn
check Nothing        = Nothing
check (Just pawns)   = find isFocused pawns



setDices :: Dices -> Int -> Int-> Player -> Dices
setDices ((Die d1, Die d2), (Die d3, Die d4)) toIdx fromIdx player_ = case player_ of
  PlayerWhite -> (setD d1 d2 toIdx fromIdx, (Die d3, Die d4))
  PlayerRed   -> ((Die d1, Die d2), setD d3 d4 toIdx fromIdx)

setD :: Int -> Int -> Int -> Int  -> (Die, Die)
setD d1 d2 toIdx fromIdx
    | d1==abs(toIdx-fromIdx)    = (Die 0, Die d2)
    | d2==abs(toIdx-fromIdx)    = (Die d1, Die 0)
    | d1+d2==abs(toIdx-fromIdx) = (Die 0, Die 0)
    | otherwise                 = (Die d1, Die d2)



resetDices :: Dices -> (Int, Int, Int, Int) -> Player -> Dices
resetDices (die1, die2) (d1, d2, d3, d4) player__ = case player__ of
 PlayerRed    -> ((Die d1, Die d2), die2)
 PlayerWhite  -> (die1, (Die d3, Die d4))


nextDSet :: [Int] -> ((Int, Int, Int, Int), [Int])
nextDSet infList = (next_, restInfList)
  where
      next_ = (head next4, next4!!1, next4!!2, next4!!3)
      next4 = take 4 infList
      restInfList = drop 4 infList


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = case chipIdx mousePos of
      Nothing -> game
      Just (qIdx, tIdx, yIdx) -> do
        let (Game oldBoard dice_ player_ gameState dValues) = game

        case gameState of
          (GameOver winner) -> game
          Running           -> do
              let (bar, quads) = resetAllFocus oldBoard
              let newBoard = updateAt qIdx (updateTrack tIdx) quads
              Game (bar, newBoard) dice_ player_ gameState dValues
          where
              updateTrack :: Int -> Quad -> Quad
              updateTrack idx tracks = updateAt idx (updatePawn yIdx) tracks

              updatePawn :: Int -> Track -> Track
              updatePawn _ Nothing = Nothing
              updatePawn idx (Just pawns) = Just $ updateAt idx updateChip pawns

              updateChip :: Pawn -> Pawn
              updateChip pawn = case (player game, pawn) of
                  (PlayerRed, PawnRed _ _)      -> pawn { isFocused=True }
                  (PlayerWhite, PawnWhite _ _)  -> pawn { isFocused=True }
                  (_, _)                        -> pawn


transformGame (EventKey (MouseButton RightButton) Up _ mousePos) game = case chipIdx mousePos of
      Nothing              -> game
      Just (qIdx, tIdx, _) -> do
        let (Game oldBoard dices_ player_ state_ dValues) = game
        let (_, quads) = oldBoard

        let focusedChip = getFocusedChip quads

        case focusedChip of
          Nothing -> Game oldBoard dices_ player_ state_ dValues
          Just (fromIdx, fChip) -> do
            let toIdx = newIdx fChip
            let newDices = setDices dices_ toIdx fromIdx player_
            let ((Die d1, Die d2), (Die d3, Die d4)) = newDices

            {-- Check movement board, fromIdx, toIdx, chip --}
            let canMove = checkMove oldBoard fromIdx toIdx fChip
            let (isBlot, bBoard) = checkBlot oldBoard toIdx fChip
            let (nextSet, newDValues) = nextDSet dValues  
--            let (dd1, dd2, dd3, dd4)  = nextSet

            case canMove of
              False -> Game oldBoard dices_ player_ state_ dValues
              _     -> do
                case (player_, whiteAllMovesDone, redAllMovesDone) of
                   (PlayerWhite, True, _) -> Game (resetAllFocus newBoard) (resetDices newDices nextSet player_) PlayerRed state_ newDValues   
                   (PlayerRed, _, True)   -> Game (resetAllFocus newBoard) (resetDices newDices nextSet player_) PlayerWhite state_ newDValues
                   _                      -> trace (show newDices) Game (resetAllFocus newBoard) newDices player_ state_ dValues
               where
                 newBoard = if isBlot
                            then pickPlacePawn bBoard (fromIdx, toIdx) fChip
                            else pickPlacePawn oldBoard (fromIdx, toIdx) fChip

                 whiteAllMovesDone = d1==0 && d2==0
                 redAllMovesDone   = d3==0 && d4==0


          where
              newIdx :: Pawn -> Int
              newIdx pawn = case pawn of
                 (PawnRed _ _)   -> tIdx + (6*qIdx)
                 (PawnWhite _ _) -> tIdx + (6*qIdx)



transformGame _ game = game



