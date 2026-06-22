module Main

import Component
import Window
import Geometry
import Event

red : Color
red = MkCol 255 0 0 255

green : Color
green = MkCol 0 255 0 255

data ColorChange = ColNoop | NewColor Color
data Appearance = ApNoop | Switch

record Circle2 where
  constructor MkCirc2
  toggle : Bool
  color : Color

circle2View : Circle2 -> View
circle2View circ =
  if circ.toggle
    then toView $ MkCircle (MkV2 150 400) 100 circ.color
    else unitView

circle2Color : ColorChange -> Circle2 -> Circle2
circle2Color (NewColor col) circ = { color := col } circ
circle2Color ColNoop circ = circ

circle2 : Component (Message1 ()) (Model1 Circle2)
circle2 = component1 (model1 (MkCirc2 True green))
                     (view1 circle2View)
                     controllerId

record Circle1 where
  constructor MkCirc1
  color : Color
  app : Bool

circle1View : Circle1 -> View
circle1View circ =
  if circ.app
    then toView $ MkCircle (MkV2 200 200) 120 circ.color
    else unitView

circle1Appearance : Appearance -> Circle1 -> Circle1
circle1Appearance ApNoop circ = circ
circle1Appearance Switch circ = { app $= not } circ

circle1Events : Event -> Circle1 -> (ColorChange, Circle1)
circle1Events (MouseClick (MkMouse BLeft pos)) circ =
  if circ.color == red
    then (NewColor red, { color := green } circ)
    else (NewColor green, { color := red } circ)
circle1Events _ circ = (ColNoop, circ)

circle2Events : Event -> Circle2 -> (Appearance, Circle2)
circle2Events (MouseClick (MkMouse BRight pos)) circ =
  (Switch, { toggle $= not } circ)
circle2Events _ circ = (ApNoop, circ)

circle1 : Component (Message1 ()) (Model1 Circle2)
          -> Component (Message1 Event <> Message1 Appearance <> Message1 Event <> Message1 ColorChange) (ProductModel (Model1 Circle1) (Model1 Circle2))
circle1 circ2 = component1 (model1 (MkCirc1 red True) <> modelOf circ2)
                           (view1 circle1View <> viewOf circ2)
                           (connect (sender1 circle1Events `monoController` prodLift {m1 = Model1 ColorChange} (controller1 circle1Appearance))
                                    (sender1 circle2Events `monoController` prodLift {m1 = Model1 Appearance} (controller1 circle2Color)))

backgroundView : () -> View
backgroundView () = layer (-99) $ MkFill (MkCol 255 255 255 255)

background : Component (Message1 Unit) (Model1 Unit)
background = component1 (model1 ()) (view1 backgroundView) controllerId

scene = background <> circle1 circle2

main : IO ()
main = windowOf scene
