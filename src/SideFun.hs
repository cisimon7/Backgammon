module SideFun where

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
