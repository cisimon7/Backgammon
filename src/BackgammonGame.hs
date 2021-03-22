module BackgammonGame where

import Components
import System.Random (randomRs, getStdGen)
import GHC.IO.Unsafe (unsafePerformIO)

backgammonGame :: Game
backgammonGame = Game { board  = initBoard
                      , dice   = ( (Die 1, Die 5), (Die 2, Die 4) )
                      , player = PlayerWhite
                      , state  = Running      
                      , dieValues = randomRs (1,5) (unsafePerformIO getStdGen) }
                      
  where initBoard = (Nothing, allQuads) :: Board

        {- SETUP GAME -}
        allQuads = [ q1, q2, q3, q4 ] :: [[Track]]

        q1 = [ set1, n, n, n, n, set2 ]
        q2 = [ n, set3, n, n, n, set4 ]
        q3 = [ set5, n, n, n, set6, n ]
        q4 = [ set7, n, n, n, n, set8 ]

        n    = Nothing

        set1 = Just [ PawnRed   0 False
                    , PawnRed   1 False ]

        set2 = Just [ PawnWhite 0 False
                    , PawnWhite 1 False
                    , PawnWhite 2 False
                    , PawnWhite 3 False
                    , PawnWhite 4 False ]

        set3 = Just [ PawnWhite 0 False
                    , PawnWhite 1 False
                    , PawnWhite 2 False ]

        set4 = Just [ PawnRed   0 False
                    , PawnRed   1 False
                    , PawnRed   2 False
                    , PawnRed   3 False
                    , PawnRed   4 False ]

        set5 = Just [ PawnWhite 0 False
                    , PawnWhite 1 False
                    , PawnWhite 2 False
                    , PawnWhite 3 False
                    , PawnWhite 4 False ]

        set6 = Just [ PawnRed   0 False
                    , PawnRed   1 False
                    , PawnRed   2 False ]

        set7 = Just [ PawnRed   0 False
                    , PawnRed   1 False
                    , PawnRed   2 False
                    , PawnRed   3 False
                    , PawnRed   4 False ]

        set8 = Just [ PawnWhite 0 False
                    , PawnWhite 1 False ]