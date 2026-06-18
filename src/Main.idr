module Main

import Graphics

red : Color
red = MkCol 255 0 0 255

blue : Color
blue = MkCol 0 0 255 255

white : Color
white = MkCol 255 255 255 255

myScene : Object
myScene = layer (-99) (MkFill white) <>
          layer 5 (MkCircle (MkV2 200 70) 120 red) <>
          MkCircle (MkV2 200 200) 70 blue

main : IO ()
main = withWindow (flip renderObject myScene)
