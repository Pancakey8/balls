module Main

import Component
import Window
import Geometry
import Event
import Data.Vect
import Data.Fin
import Debug.Trace

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

data DetonateSignal : Nat -> Type where
  Wait : DetonateSignal n
  Go : (k : Fin n) -> DetonateSignal n

record Circles (n : Nat) where
  constructor MkCircles
  states : Vect n Bool

circlesView : (n : Nat) -> Model [Circles n] -> View
circlesView n [(MkCircles states)] =
  foldl (\v, (i, st) =>
    case st of
      True => v <> (MkCircle (MkV2 (50 * cast (finToNat i) + 50) 40) 25 red)
      False => v) unitView (zip (allFins n) states)

detonationTrigger : (n : Nat) -> Controller (Event, DetonateSignal n) (Unit, DetonateSignal n) []
detonationTrigger 0 = liftState (arrow (const ((), Wait)))
detonationTrigger (S n) = MkController $ \(ev, det), st =>
  case det of
    Wait => case ev of
      MouseClick _ => (((), Go FZ), st)
      _ => (((), Wait), st)
    _ => (((), det), st)

detonationHandler : (n : Nat) -> Controller (DetonateSignal n) (DetonateSignal n) [Circles n]
detonationHandler 0 = liftState (arrow (const Wait))
detonationHandler (S n) = MkController $ \ev, [circ] =>
  case ev of
    Wait => (Wait, [circ])
    Go k =>
      let circ' = trace (show k) $ {states $= updateAt k not} circ
      in case strengthen k of
        Just k => (Go (FS k), [circ'])
        Nothing => (Wait, [circ'])

circlesHandler : (n : Nat) -> Controller Event Unit [DetonateSignal n, Circles n]
circlesHandler n = loop $ liftState $ liftState (detonationTrigger n) >>> second (detonationHandler n)

circles : (n : Nat) -> Component Event Unit [DetonateSignal n, Circles n]
circles n = MkComponent (\[_, circ] => circlesView n [circ]) (circlesHandler n)

background : Component Unit Unit []
background = MkComponent (\[] => toView (MkFill white)) (arrow id)

main : IO ()
main = windowOf (liftState' (lmap (const ()) background) &&& circles 10) [Wait, MkCircles (replicate 10 True)]
