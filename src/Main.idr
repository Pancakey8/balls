module Main

import Component
import Window
import Geometry
import Event
import Control.Monad.Identity
import Data.Nat
import Data.Vect
import Data.Fin

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

record Circles (n : Nat) where
  constructor MkCircles
  circs : Vect n Bool

viewCircles : {n : Nat} -> Model [Circles n] -> View
viewCircles [MkCircles circs] = 
  foldl (\v, (i, b) =>
    if b
      then v <> MkCircle (MkV2 (40 * cast (finToNat i) + 40) 60) 15 red
      else v) unitView $ zip (allFins n) circs

record CircleToggler (n : Nat) where
  constructor MkToggler
  bools : Vect n Bool

{n : Nat} ->
Iterative IO n (CircleToggler n) (Vect n Bool) where
  iterStep i (MkToggler circs) = do
    putStrLn $ "Toggling " ++ show i
    pure $ Just $ MkToggler $
      updateAt i not circs

  project (MkToggler circs) = circs

toggleTask : {n : Nat} -> Vect n Bool ->
            IterMachine IO n (CircleToggler n) (Vect n Bool)
toggleTask circs = makeIter (MkToggler circs)

toggleCircles : {n : Nat} ->
                ControllerT IO Event Unit [Circles n, JobSlot IO 21 (Vect n Bool)]
toggleCircles = MkControllerT $ \ev, [state, slot] => do
  (results, slot) <- evaluate slot
  -- let state = case results of
  --               [] => state
  --               (x :: xs) => MkCircles x
  case ev of
    MouseClick (MkMouse BLeft pos) => do
      (Left mach) <- iterateN 10 (toggleTask state.circs)
        | Right _ => pure ((), [state, slot])
      let state = MkCircles (read mach)
      let slot = throttle (toggleTask state.circs) slot
      pure ((), [state, slot])
    others => do
      putStrLn "No event"
      pure ((), [state, slot])

circleComp : {n : Nat} -> ComponentT IO Event Unit [Circles n, JobSlot IO 21 (Vect n Bool)]
circleComp = MkComponentT (\[state, slot] => viewCircles [state]) toggleCircles

main : IO ()
main = windowOf circleComp [MkCircles (replicate 20 True), emptySlot]
