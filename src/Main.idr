module Main

import Component
import Window
import Geometry
import Event

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

data AnimTick = Tick Double
data ColorState = WaitBlue | WaitRed

record Circle1 where
  constructor MkCircle1
  color : Color

viewCircle1 : Model [Circle1] -> View
viewCircle1 [circ] =
  toView $ MkCircle (MkV2 200 200) 40 circ.color

eventHandler : Controller (Event, AnimTick) (Unit, AnimTick) [Circle1]
eventHandler = MkController $ \(ev, (Tick n)), [circ] =>
  case ev of
    GameTick delta => 
      let n' = delta + n
      in (((), Tick n'), [circ])
    _ => (((), Tick n), [circ])

switchBlue : Controller (ColorState, AnimTick) (ColorState, AnimTick) [Circle1]
switchBlue = MkController $ \(colSt, Tick n), [circ] =>
  case colSt of
    WaitBlue =>
      if n >= 2
        then ((WaitRed, Tick (n - 2)), [{ color := blue } circ])
        else ((WaitBlue, Tick n), [circ])
    other => 
      ((other, Tick n), [circ])

switchRed : Controller (ColorState, AnimTick) (ColorState, AnimTick) [Circle1]
switchRed = MkController $ \(colSt, Tick n), [circ] =>
  case colSt of
    WaitRed =>
      if n >= 2
        then ((WaitBlue, Tick (n - 2)), [{ color := red } circ])
        else ((WaitRed, Tick n), [circ])
    other => 
      ((other, Tick n), [circ])

alternatingHandler : Controller (Unit, ColorState) (Unit, ColorState) [AnimTick, Circle1]
alternatingHandler = second (loop (liftState $ switchBlue >>> switchRed))

eventLoop : Controller Event Unit [AnimTick, Circle1]
eventLoop = loop (liftState eventHandler)

alternatingLoop : Controller Unit Unit [ColorState, AnimTick, Circle1]
alternatingLoop = loop (liftState alternatingHandler)

circle : Component Event Unit [ColorState, AnimTick, Circle1]
circle = MkComponent (\[_, _, circ] => viewCircle1 [circ])
         $ liftState eventLoop >>> alternatingLoop

main : IO ()
main = windowOf circle [WaitBlue, Tick 0, MkCircle1 red]

