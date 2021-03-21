module SideFun where

  
import Components

  
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i tryUpdate list
  | i > length list - 1 || i < 0  = list
  | null list                     = list
  | otherwise                     = a ++ zs

  where (a, bs) = splitAt i list
        zs = case bs of
          [] -> []
          (c:ds) -> tryUpdate c : ds
        

removeAt :: Int -> [a] -> [a]
removeAt i list = as ++ zs
    
    where (as, bs) = splitAt i list
          zs = case bs of
            [] -> []
            (_:cs) -> cs


setPt :: Board -> Board
setPt (bar, quads) = (bar, map (map organiseTrack) quads)


organiseTrack :: Track -> Track
organiseTrack track = case track of
  Nothing -> Nothing
  Just pawns -> Just [ pawn { pt=i } | (pawn, i) <- zip pawns [0..(length pawns - 1)] ]


{- Removes a pawn at a specific index from a track -}
removePawn :: Int -> Track -> Track
removePawn idx track = case track of
  Nothing -> Nothing
  (Just pawns)
    | null newPawns -> Nothing
    | otherwise     -> organiseTrack $ Just newPawns
    where newPawns = removeAt idx pawns


{- Adds a pawn at a given pt in a track -}
addPawn :: Int -> Pawn -> Track -> Track
addPawn _ pawn track = case track of
  Nothing -> Just [pawn { pt=0 }]
  Just _  -> newTrack
    where
        l = length track
        newTrack = case organiseTrack track of
            Nothing -> Nothing
            (Just orderedPawns) -> Just (orderedPawns ++ [pawn { pt=l }])


{- returns a tuple of quad position and track position -}
getTrackId :: Int -> (Int, Int)
getTrackId z = (floor (fromIntegral z /6), mod z 6)