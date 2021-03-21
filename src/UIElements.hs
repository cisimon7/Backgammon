module UIElements where

import Graphics.Gloss.Interface.IO.Game ( makeColor, Color )


backgroundColor :: Color
backgroundColor = makeColor (0/255) (0/255) (0/255) 1

quadColor :: Color
quadColor = makeColor (255/255) (191/255) (0/255) (255/255)

redChipColor :: Color
redChipColor = makeColor (222/255) (49/255) (99/255) (255/255)

whiteChipColor :: Color
whiteChipColor = makeColor (253/255) (254/255) (254/255) (255/255)

evenTrackColor :: Color
evenTrackColor = makeColor (171/255) (235/255) (198/255) (255/255)

oddTrackColor :: Color
oddTrackColor = makeColor (171/255) (178/255) (185/255) (255/255)

focusColor :: Color
focusColor = makeColor (93/255) (173/255) (226/255) (255/255)