module Ch1 (work) where

import Graphics.Gloss

bottomCircle :: Color -> Picture
bottomCircle c = Color c (translate 0 (-45) (circleSolid 40))

topCircle :: Color -> Picture
topCircle c = Color c (translate 0 (45) (circleSolid 40))

frame = rectangleWire 100 200

trafficLight :: Bool -> Picture
trafficLight True = pictures [(bottomCircle black), (topCircle green), frame]
trafficLight False = pictures [(bottomCircle red), (topCircle black), frame]

trafficLights :: Integer -> Bool -> Picture
trafficLights 0 b = blank
trafficLights n b = pictures [trafficLight b, (translate 120 0 (trafficLights (n-1) b))]

trafficAnimation :: Float -> Picture
trafficAnimation t
  | round (t/3) `mod` 2 == 0  = trafficLights 3 True
  | otherwise                 = trafficLights 3 False

work :: IO ()
work = animate windowHandle bgColor animation
      where 
        windowHandle = (InWindow "Nice Window" (200, 200) (10, 10))
        bgColor = white
        animation = trafficAnimation

