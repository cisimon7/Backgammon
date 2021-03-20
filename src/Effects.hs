{-# LANGUAGE BlockArguments #-}

module Effects where

import SideFun
import Pictures
import Components
import Debug.Trace
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
          | x>bWidth && x<(qWidth+bWidth) && y>0 && y<qHeight        = Just 3
          | x<(-bWidth) && x>(-qWidth-bWidth) && y>0 && y<qHeight    = Just 2
          | x<(-bWidth) && x>(-qWidth-bWidth) && y>(-qHeight) && y<0 = Just 1
          | x>bWidth && x<(qWidth+bWidth) && y>(-qHeight) && y<0     = Just 0
          | otherwise = Nothing

        trackIdx :: (Float, Float) -> Maybe Int
        trackIdx (x,y)
          | x>bWidth && x<(qWidth+bWidth) && y>0 && y<qHeight        = Just $ floor ((x-bWidth)/pWidth)
          | x<(-bWidth) && x>(-qWidth-bWidth) && y>0 && y<qHeight    = Just $ 5 - floor (abs(x+bWidth)/pWidth)
          | x<(-bWidth) && x>(-qWidth-bWidth) && y>(-qHeight) && y<0 = Just $ floor (abs(x+bWidth)/pWidth)
          | x>bWidth && x<(qWidth+bWidth) && y>(-qHeight) && y<0     = Just $ 5 - floor ((x-bWidth)/pWidth)
          | otherwise                                                = Nothing

        yIdx :: (Float, Float) -> Maybe Int
        yIdx (_,y)
          | y>0 && y<qHeight    = Just $ 5 - floor (y/(s*pWidth))
          | y>(-qHeight) && y<0 = Just $ abs (floor (abs y/(s*pWidth)) - 5)
          | otherwise           = Nothing


transformGame :: Event -> Board -> Board
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) oldBoard = case chipIdx mousePos of
      Nothing -> oldBoard
      Just (qIdx, tIdx, yIdx) -> do
        let (bar, quads) = resetAllFocus oldBoard
        let newQuads = updateAt qIdx (updateTrack tIdx) quads
        (bar, newQuads)
          where
              updateTrack :: Int -> Quad -> Quad
              updateTrack idx tracks = updateAt idx (updatePawn yIdx) tracks

              updatePawn :: Int -> Track -> Track
              updatePawn _ Nothing = Nothing
              updatePawn idx (Just pawns) = Just $ updateAt idx updateChip pawns

              updateChip :: Pawn -> Pawn
              updateChip pawn = pawn { isFocused=True }


transformGame (EventKey (MouseButton RightButton) Up _ mousePos) oldBoard = case chipIdx mousePos of
      Nothing              -> oldBoard
      Just (qIdx, tIdx, _) -> do
        let (_, quads) = oldBoard

        let focusedChip = getFocusedChip quads
        
        case focusedChip of
          Nothing -> oldBoard
          Just (fromIdx, fChip) -> do
            let toIdx = newIdx fChip
            let newBoard = pickPlacePawn oldBoard (fromIdx, toIdx) fChip
            resetAllFocus newBoard

          where
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

              newIdx :: Pawn -> Int
              newIdx pawn = case pawn of
                 (PawnRed _ _)   -> tIdx + (6*qIdx)
                 (PawnWhite _ _) -> tIdx + (6*qIdx)


transformGame _ boardState = boardState