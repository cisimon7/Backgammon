module Components where


data Game = Game { board  :: Board
                 , player :: Player
                 , state  :: State
                 }
--                 deriving (Eq, Show)

data Player = PlayerRed | PlayerWhite
  deriving (Eq, Show)

data State  = Running | GameOver (Maybe Player)
  deriving (Eq, Show)

type Board = (Bar, [[Quad]])

type Quad = [Point_]

type Bar = Maybe [Checker]

type Point_ = Maybe [Checker]

data Checker = CheckerRed { pt::Float } | CheckerWhite { pt::Float } 
  deriving (Eq, Show)

screenWidth, screenHeight :: Int
screenWidth  = 1000
screenHeight = 700

-- Checker Radius
chkRad :: Float
chkRad = 35

-- Point Dimensions
pWidth, pHeight :: Float
pWidth = 2*chkRad
pHeight = fromIntegral screenHeight*0.85*0.5

-- Quad Dimensions
qWidth, qHeight :: Float
qWidth = 6*pWidth
qHeight = fromIntegral screenHeight*0.5

-- Bar Dimensions
bWidth, bHeight :: Float
bWidth = chkRad
bHeight = fromIntegral screenHeight

