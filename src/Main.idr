module Main

import Component
import Window
import Geometry
import Event
import Debug.Trace
import Control.Monad.Identity
import Data.Nat

red = MkCol 255 0 0 255
blue = MkCol 0 0 255 255
white = MkCol 255 255 255 255

predMaybe : Nat -> Maybe Nat
predMaybe Z = Nothing
predMaybe (S k) = Just k

record Circles where
  constructor MkCircles
  count : Nat

viewCircles : Model [Circles] -> View
viewCircles [circ] =
  case predMaybe circ.count of
    Nothing => unitView
    Just n => foldl (\v, i => v <> MkCircle (MkV2 (80 * cast i + 80) 60) 30 red) unitView [0..n]

data CircleDelta = Increment | Decrement

events : Controller Event (Either CircleDelta Unit) []
events = MkControllerT $ \ev, [] =>
  case ev of
    MouseClick (MkMouse BLeft pos) =>
      pure (Left Decrement, [])
    MouseClick (MkMouse BRight pos) =>
      pure (Left Increment, [])
    _ => pure (Right (), [])

logger : ControllerT IO CircleDelta Unit [Circles]
logger = MkControllerT $ \delta, [circ] => do
  case delta of
    Increment => putStrLn $ "Incremented to " ++ show circ.count
    Decrement => putStrLn $ "Decremented to " ++ show circ.count
  pure ((), [circ])

circleOperate : Controller CircleDelta Unit [Circles]
circleOperate = MkControllerT $ \delta, [circ] =>
  case delta of
    Increment =>
      pure ((), [{ count $= S } circ])
    Decrement =>
      pure ((), [{ count $= pred } circ])

circlesController : ControllerT IO Event Unit [Circles]
circlesController = hoist (liftState events) >>> left (logger &&& (hoist circleOperate)) >>> liftState (arrow (const ()))

circles : ComponentT IO Event Unit [Circles]
circles = MkComponentT viewCircles circlesController

main : IO ()
main = windowOf circles [MkCircles 5]
