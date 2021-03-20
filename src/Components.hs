module Components where


data Game = Game { board  :: Board
                 , player :: Player
                 , state  :: State
                 } deriving (Eq, Show)

data Player = PlayerRed | PlayerWhite
  deriving (Eq, Show)

data State  = Running | GameOver (Maybe Player)
  deriving (Eq, Show)

type Board = (Bar, [Quad])

type Quad = [Track]

type Bar = Maybe [Pawn]

type Track = Maybe [Pawn]

data Pawn =    PawnRed { pt::Int, isFocused::Bool } 
           | PawnWhite { pt::Int, isFocused::Bool }
  deriving (Eq, Show)

-- Game Window Dimensions
screenWidth, screenHeight :: Int
screenWidth  = 1000
screenHeight = 700

-- Checker Radius
pawnRadius :: Float
pawnRadius = 35

-- Point Dimensions
pWidth, pHeight :: Float
pWidth = 2*pawnRadius
pHeight = fromIntegral screenHeight*0.85*0.5

-- Quad Dimensions
qWidth, qHeight :: Float
qWidth = 6*pWidth
qHeight = fromIntegral screenHeight*0.5

-- Bar Dimensions
bWidth, bHeight :: Float
bWidth = pawnRadius
bHeight = fromIntegral screenHeight

