module Rules where


import SideFun
import Components
{--
    Create a list of rules that can be sort of associative and if one fails,
    then everything fails
--}

{-- Switch Player --}
{-- Rearrange pawn heights --}

setBarChips :: Board -> Int -> Board
setBarChips (bar, board_) tIdx_ = do
  let (qIdx, tIdx) = getTrackId tIdx_
  let bTrack = board_ !! qIdx !! tIdx
  
--  let nBoard = map (map (setBChips)) board_
  
  let newBoard = updateAt qIdx (updateAt tIdx (removePawn 0)) board_
  case bTrack of
    Nothing -> (bar, board_)
    (Just pawns) -> if null pawns 
                    then  (bar, board_) 
                    else case bar of
                            Nothing ->  (Just [head pawns], newBoard)
                            (Just bChips) -> (Just (head pawns:bChips), newBoard)
  
--  where
--      setBChips :: Track -> Track
--      setBChips = undefined

{- Count number of type_pawns in a track -}
trackContent :: Track -> (Int, Int)
trackContent = filterTrack . transTrack
  where
    filterTrack :: [Int] -> (Int, Int)
    filterTrack chips = (length whites, length reds)
      where 
        whites = filter (== 0) chips
        reds   = filter (== 1) chips
    
    transTrack :: Track -> [Int]
    transTrack Nothing = [] 
    transTrack (Just pawns) = map transPawn pawns 
    
    transPawn :: Pawn -> Int
    transPawn pawn_ = case pawn_ of
      (PawnWhite _ _) -> 0
      (PawnRed _ _)   -> 1


{- Check direction of Movement -}
{- Check if next track is open -}
checkMove :: Board -> Int -> Int -> Pawn -> Bool
checkMove board_ from to pawn = case pawn of
  (PawnRed _ _)   -> to > from && (trackIsOpen pawn to)
  (PawnWhite _ _) -> from > to && (trackIsOpen pawn to)
  where
      trackIsOpen :: Pawn -> Int -> Bool
      trackIsOpen chip tIdx_ = do
        let (qIdx, tIdx) = getTrackId tIdx_
        let track = snd board_ !! qIdx !! tIdx
        let (whiteCount, redCount) = trackContent track
        case chip of
          (PawnWhite _ _) -> redCount<=1
          (PawnRed _ _)   -> whiteCount<=1

      
checkBlot :: Board -> Int -> Pawn -> (Bool, Board)
checkBlot board_ toIdx chip = updateBlots chip toIdx
  where  
    updateBlots :: Pawn -> Int -> (Bool, Board)
    updateBlots chip_ tIdx_ = do
      let (qIdx, tIdx) = getTrackId tIdx_
      let track = snd board_ !! qIdx !! tIdx
      let (whitCount, redCount) = trackContent track
      case (chip_, redCount) of
        (PawnWhite _ _, 1)  -> (True, blotUpdatedBoard qIdx tIdx board_) 
        (PawnRed _ _, _)    -> case whitCount of
            1 -> (True, blotUpdatedBoard qIdx tIdx board_) 
            _ -> (False, board_)
        _ -> (False, board_)
        
      where
        blotUpdatedBoard :: Int -> Int -> Board -> Board
        blotUpdatedBoard qIdx tIdx (bar, oldBoard) = setBarChips (bar, updateAt qIdx (updateAt tIdx setBlot) oldBoard) (tIdx + 6*qIdx)
        setBlot :: Track -> Track
        setBlot track = case track of
          Nothing -> Nothing
          (Just pawns) -> Just $ map (\chip' -> chip' { pt=(-1) }) pawns
        


getPips :: Int -> Int -> [Int]
getPips d1 d2 = [d1, d2, d1+d2]