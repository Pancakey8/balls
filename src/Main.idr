module Main

import Component
import Window
import Geometry
import Event
import Data.Vect
import Data.Fin
import Data.Fin.Properties
import Debug.Trace

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

data DetonateWait : Nat -> Type where
  Wait : DetonateWait n

data DetonateGo : Nat -> Type where
  Go : DetonateGo n

record Circles (n : Nat) where
  constructor MkCircles
  states : Vect n Bool

circlesView : (n : Nat) -> Model [Circles (S n)] -> View
circlesView n [(MkCircles states)] =
  foldl (\v, (i, st) =>
    case st of
      True => v <> (MkCircle (MkV2 (50 * cast (finToNat i) + 50) 40) 25 red)
      False => v) unitView (zip (allFins (S n)) states)

detonationTrigger : (n : Nat) -> Controller (Event, DetonateWait (S n)) (Unit, Either (DetonateWait (S n)) (DetonateGo (S n))) []
detonationTrigger 0 = liftState (arrow (const ((), Left Wait)))
detonationTrigger (S n) = MkController $ \(ev, det), st =>
  case ev of
    MouseClick _ => (((), Right Go), st)
    _ => (((), Left Wait), st)

public export
data RecStep : Nat -> Type -> Type where
  Pure : a -> RecStep n a
  Next : (a -> a) -> Inf (RecStep n a) -> RecStep (S n) a

recMap : {n : Nat} -> (a -> a) -> RecStep n a -> RecStep n a
recMap f (Pure v) = Pure (f v)
recMap f (Next g next) = Next (f . g) next

fix : (k : Nat) -> {n : Nat} -> RecStep (k + n) a -> Either (RecStep n a) a
fix _ (Pure x) = Right x
fix Z comp = Left comp
fix (S k) (Next f next) =
  case fix k next of
    Left comp => Left (recMap f comp)
    Right v => Right (f v)

factorial : (n : Nat) -> RecStep n Int
factorial 0 = Pure 1
factorial (S n) = Next (\k => cast (S n) * k) (factorial n)

toggleCircles : (n : Nat) -> Circles (S n) -> RecStep n (Circles (S n))
toggleCircles n circ = toggle n {pf=reflexive} circ
  where
    toggle : (k : Nat) -> {auto pf : LTE (S k) (S n)} -> Circles (S n) -> RecStep k (Circles (S n))
    toggle Z circ = Pure $ {states $= updateAt last not} circ
    toggle (S k) {pf=LTESucc pf} circ = Next {states $= updateAt (complement $ natToFinLT (S k)) not}
                                        $ toggle k {pf=lteSuccRight pf} circ

wait : {n : Nat} -> Controller a (DetonateWait n) []
wait = arrow (const Wait)

circles : (n : Nat) -> Component Event Unit [DetonateWait (S n), Circles (S n)]
circles n = MkComponent (\[_, circ] => circlesView n [circ]) (loop $ liftState (detonationTrigger n >>> second (wait \|/ wait)))

background : Component Unit Unit []
background = MkComponent (\[] => toView (MkFill white)) (arrow id)

main : IO ()
main = windowOf (liftState (lmap (const ()) background) &&& (circles 9)) [Wait, MkCircles (replicate 10 True)]
