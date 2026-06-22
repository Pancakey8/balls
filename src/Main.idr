module Main

import Component
import Window
import Geometry
import Event

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

record Circle1 where
  constructor MkCircle1
  colorSwitch : Bool

viewCircle1 : Model [Circle1] -> View
viewCircle1 [circ] = toView $ MkCircle (MkV2 200 200) 40 (if circ.colorSwitch
                                                          then red
                                                          else blue)

data ColorSignal = Switch | Stay

eventsCircle1 : Controller Event ColorSignal [Circle1]
eventsCircle1 = MkController $ \ev, [circ] =>
  case ev of
    MouseClick (MkMouse BLeft (MkV2 x y)) =>
      if pow (x - 200) 2 + pow (y - 200) 2 <= 1600
        then (Switch, [{ colorSwitch $= not } circ])
        else (Stay, [circ])
    _ => (Stay, [circ])

record Circle2 where
  constructor MkCircle2
  color : Color
  visible : Bool

viewCircle2 : Model [Circle2] -> View
viewCircle2 [circ] =
  if circ.visible
    then toView $ MkCircle (MkV2 100 300) 60 circ.color
    else unitView

eventsCircle2 : Controller Event Unit [Circle2]
eventsCircle2 = MkController $ \ev, [circ] =>
  case ev of
    MouseClick (MkMouse BLeft (MkV2 x y)) =>
      if pow (x - 100) 2 + pow (y - 300) 2 <= 3600
        then ((), [{ visible $= not } circ])
        else ((), [circ])
    _ => ((), [circ])

colorCircle2 : Controller ColorSignal Unit [Circle2]
colorCircle2 = MkController $ \ev, [circ] =>
  case ev of
    Switch =>
      if circ.color == blue
        then ((), [{ color := red } circ])
        else ((), [{ color := blue } circ])
    Stay => ((), [circ])

background : Component Unit Unit []
background = MkComponent (\[] => layer (-99) (MkFill white))
                         (arrow id)

composedCtrl : Controller Event Unit [Circle1, Circle2]
composedCtrl = 
  (liftState' _ eventsCircle1 &&& liftState [Circle1] eventsCircle2)
    >>> lmap fst (liftState [Circle1] colorCircle2)

jointCircle : Component Event Unit [Circle1, Circle2]
jointCircle = MkComponent (\[c1, c2] => viewCircle1 [c1] <> viewCircle2 [c2]) composedCtrl

screen = liftState' _ (lmap (const ()) background) &&& jointCircle

main : IO ()
main = windowOf screen [MkCircle1 False, MkCircle2 red True]
