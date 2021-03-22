module Rules where


import SideFun
import Effects
import Components
import Debug.Trace
{--
    Create a list of rules that can be sort of associative and if one fails,
    then everything fails
--}

{-- Switch Player --}
{-- Rearrange pawn heights --}

--  where
--      setBChips :: Track -> Track
--      setBChips = undefined


{- Checks direction of Movement for a given pawn -}
{- Check if pawn can move from one track id to the next track id: openTrackTest -}
getAllowedMoves :: Board -> Die -> Die -> Pawn -> Bool
getAllowedMoves board_ fTo dTo pawn = case pawn of
  (PawnRed _ _)   -> (x dTo) > (x fTo) && (trackIsOpen pawn (x dTo))
  (PawnWhite _ _) -> (x fTo) > (x dTo) && (trackIsOpen pawn (x fTo))
  where
      trackIsOpen :: Pawn -> Int -> Bool
      trackIsOpen chip tIdx_ = do
        let (qIdx, tIdx) = getTrackId tIdx_
        let track = snd board_ !! qIdx !! tIdx
        let (whiteCount, redCount) = trackContent track

        case chip of
          (PawnWhite _ _) -> redCount<=1
          (PawnRed _ _)   -> whiteCount<=1


{- Takes current board, the values of the two dice and a selected pawn
   and returns the possible trackIds to move to -}
goodMoves :: Board -> (Die, Die) -> [Int]
goodMoves (bar, quads) (Die d1, Die d2) = do
    case getFocusedChip quads of
      Nothing           -> []
      Just (fId, fChip) -> do
        case fChip of
            (PawnWhite _ _) -> filter (\to -> to <= 23 && to>=0 && checkMove (bar, quads) fId to fChip) [fId-d1, fId-d2, fId-d1-d2]
            (PawnRed _ _)   -> filter (\to -> to <= 23 && to>=0 && checkMove (bar, quads) fId to fChip) [d1+fId, d2+fId, d1+d2+fId]
-- && checkMove (bar, quads) fId to fChip

getPips :: Int -> Int -> [Int]
getPips d1 d2 = [d1, d2, d1+d2]