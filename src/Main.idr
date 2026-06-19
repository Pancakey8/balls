module Main

import Debug.Trace
import Graphics

red : Color
red = MkCol 255 0 0 255

blue : Color
blue = MkCol 0 0 255 255

white : Color
white = MkCol 255 255 255 255

record GameState where
  constructor MkGame
  toggle : Bool

myScene : GameState -> Object
myScene st = layer (-99) (MkFill white) <>
             layer 5 (MkCircle (MkV2 200 70) 120 (if st.toggle then red else blue)) <>
             MkCircle (MkV2 200 200) 70 blue

event : Event -> GameState -> GameState
event (MouseClick (MkMouse BLeft pos)) st = { toggle $= not } st
event _ st = st

handleGame : Window -> GameState -> IO ()
handleGame win st = events win st >>= \st' => renderObject win (myScene st')
                                              >> frame (handleGame win st')
  where
    events : Window -> GameState -> IO GameState
    events win st = trace "Events" $
      pollEvent win >>= \case
        Just ev => events win (event ev st)
        Nothing => pure st

main : IO ()
main = withWindow $ \win => frame (handleGame win (MkGame False))
