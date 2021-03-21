{-# LANGUAGE BlockArguments #-}

module Effects where

import Rules
import SideFun
import Pictures
import Components
import Debug.Trace
import BackgammonGame
import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game (Event)

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


transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = case chipIdx mousePos of
      Nothing -> game
      Just (qIdx, tIdx, yIdx) -> do
        let (Game oldBoard dice_ player_ gameState) = game

        case gameState of
          (GameOver winner) -> game
          Running           -> do
              let (bar, quads) = resetAllFocus oldBoard
              let newBoard = updateAt qIdx (updateTrack tIdx) quads
              Game (bar, newBoard) dice_ player_ gameState
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
        let (Game oldBoard dice_ player_ state_) = game
        let (_, quads) = oldBoard

        let focusedChip = getFocusedChip quads

        case focusedChip of
          Nothing -> Game oldBoard dice_ player_ state_
          Just (fromIdx, fChip) -> do
            let toIdx = newIdx fChip

            {-- Check movement board, fromIdx, toIdx, chip --}
            let canMove = checkMove oldBoard fromIdx toIdx fChip
            let (isBlot, bBoard) = checkBlot oldBoard toIdx fChip

            case canMove of
              False -> Game oldBoard dice_ player_ state_
              _     -> do
                case player_ of
                   PlayerWhite -> Game (resetAllFocus newBoard) dice_ PlayerRed state_
                   PlayerRed   -> Game (resetAllFocus newBoard) dice_ PlayerWhite state_
               where
                 newBoard = if isBlot
                            then pickPlacePawn bBoard (fromIdx, toIdx) fChip
                            else pickPlacePawn oldBoard (fromIdx, toIdx) fChip


          where
              newIdx :: Pawn -> Int
              newIdx pawn = case pawn of
                 (PawnRed _ _)   -> tIdx + (6*qIdx)
                 (PawnWhite _ _) -> tIdx + (6*qIdx)


transformGame _ game = game



