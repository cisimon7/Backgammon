module Components where


type Dices = ((Die,Die), (Die,Die))

data Game = Game { board  :: Board
                 , dice   :: Dices
                 , player :: Player
                 , state  :: State
                 , dieValues :: [Int]
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

newtype Die = Die { x::Int }
  deriving (Eq, Show)

-- Game Window Dimensions
screenWidth, screenHeight :: Int
screenWidth  = 1000
screenHeight = 700

expandWidth :: Float
expandWidth = 20

-- Checker Radius
pawnRadius :: Float
pawnRadius = 35

s :: Float
s = 0.85

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
bWidth = pawnRadius*2
bHeight = fromIntegral screenHeight

