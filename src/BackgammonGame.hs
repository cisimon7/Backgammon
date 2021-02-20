module BackgammonGame where

import Components

backgammonGame :: Game
backgammonGame = Game { board  = (Nothing, [ [redOuter,   redHome]
                                           , [whiteOuter, whiteHome] ])
                      , player = PlayerWhite
                      , state  = Running
                      }
  where redHome    = [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        redOuter   = redHome
        whiteHome  = redHome
        whiteOuter = redHome