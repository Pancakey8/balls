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

data ColorChange = NewColor Color | Noop

record Circle2 where
  constructor MkCirc2
  toggle : Bool
  color : Color

circle2View : Circle2 -> View
circle2View circ =
  if circ.toggle
    then toView $ MkCircle (MkV2 150 400) 100 circ.color
    else unitView

circle2Events : Event -> Circle2 -> Circle2
circle2Events (MouseClick (MkMouse BRight pos)) circ =
  { toggle $= not } circ
circle2Events _ circ = circ

circle2Color : ColorChange -> Circle2 -> Circle2
circle2Color (NewColor col) circ = { color := col } circ
circle2Color Noop circ = circ

circle2 : Component (Message1 Event <> Message1 ColorChange) (Model1 Circle2)
circle2 = component1 (model1 (MkCirc2 True green))
                     (view1 circle2View)
                     (controller1 circle2Events <> controller1 circle2Color)

circle1View : Circle1 -> View
circle1View circ = toView $ MkCircle (MkV2 200 200) 120 circ.color

circle1Events : Event -> Circle1 -> (ColorChange, Circle1)
circle1Events (MouseClick (MkMouse BLeft pos)) circ =
  if circ.color == red
    then (NewColor red, { color := green } circ)
    else (NewColor green, { color := red } circ)
circle1Events _ circ = (Noop, circ)

forwardTo : {s1 : MessageType} -> {ev : Type} -> {st : Type} -> Controller s1 m1 -> {auto els : Elems out s1} ->
            (ev -> st -> (out, st)) -> Controller (Message1 ev <> s1) (ProductModel (Model1 st) m1)
forwardTo cont {els} hdl =
  Handle $ \msg, mdl =>
    let (l, r) = splitModel {m1=Model1 st} mdl
    in case splitMessage {s1=Message1 ev} msg of
      Left msg1 =>
        let (ev, l') = hdl (getMsg msg1) (getData l)
        in (model1 l') <> broadcast ev r cont
      Right msg2 => l <> run msg2 r cont

circle1 : Component (Message1 Event <> Message1 ColorChange) (Model1 Circle2)
          -> Component (Message1 Event <> Message1 Event <> Message1 ColorChange) (ProductModel (Model1 Circle1) (Model1 Circle2))
circle1 circ2 = component1 (model1 (MkCirc1 red) <> modelOf circ2)
                           (view1 circle1View <> viewOf circ2)
                           (forwardTo (controllerOf circ2) circle1Events)

backgroundView : () -> View
backgroundView () = layer (-99) $ MkFill (MkCol 255 255 255 255)

background : Component (Message1 Unit) (Model1 Unit)
background = component1 (model1 ()) (view1 backgroundView) controllerId

scene = background <> circle1 circle2

main : IO ()
main = windowOf scene
