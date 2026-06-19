module Main

import Component
import Window
import Geometry
import Event

red : Color
red = MkCol 255 0 0 255

green : Color
green = MkCol 0 255 0 255

record Circle1 where
  constructor MkCirc1
  color : Color

circle1View : Circle1 -> View
circle1View circ = toView $ MkCircle (MkV2 200 200) 120 circ.color

circle1Events : Event -> Circle1 -> Circle1
circle1Events (MouseClick (MkMouse BLeft pos)) circ =
  if circ.color == red
    then { color := green } circ
    else { color := red } circ
circle1Events _ circ = circ

circle1 : Component (Message1 Event) (Model1 Circle1)
circle1 = component1 (model1 (MkCirc1 red))
                     (view1 circle1View)
                     (controller1 circle1Events)

record Circle2 where
  constructor MkCirc2
  toggle : Bool

circle2View : Circle2 -> View
circle2View circ =
  if circ.toggle
    then toView $ MkCircle (MkV2 150 400) 100 (MkCol 40 40 40 255)
    else unitView

circle2Events : Event -> Circle2 -> Circle2
circle2Events (MouseClick (MkMouse BRight pos)) circ =
  { toggle $= not } circ
circle2Events _ circ = circ

circle2 : Component (Message1 Event) (Model1 Circle2)
circle2 = component1 (model1 (MkCirc2 True))
                     (view1 circle2View)
                     (controller1 circle2Events)

backgroundView : () -> View
backgroundView () = layer (-99) $ MkFill (MkCol 255 255 255 255)

background = component1 (model1 ()) (view1 backgroundView) controllerId

scene = background <> circle1 <> circle2

main : IO ()
main = windowOf scene
