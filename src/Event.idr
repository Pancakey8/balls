module Event

import Window
import Geometry

public export
data Button = BLeft | BMiddle | BRight

public export
record MouseEvent where
  constructor MkMouse
  button : Button
  pos : Vec2

public export
data Event = MouseHold MouseEvent
           | MouseRelease MouseEvent
           | MouseClick MouseEvent
           | GameTick Double

public export
data EventKind = KMouseHold
               | KMouseRelease
               | KMouseClick

public export
data PrimEvent : Type where [external]

%foreign "browser:lambda:hasEvent"
prim_hasEvent : PrimEvent -> Int
%foreign "browser:lambda:getEventKind"
prim_eventKind : PrimEvent -> Int
%foreign "browser:lambda:getMouseEventButton"
prim_mouseEventButton : PrimEvent -> Int
%foreign "browser:lambda:getMouseEventX"
prim_mouseEventX : PrimEvent -> Double
%foreign "browser:lambda:getMouseEventY"
prim_mouseEventY : PrimEvent -> Double

hasEvent : PrimEvent -> Bool
hasEvent ev =
  case prim_hasEvent ev of
    0 => False
    1 => True
    _ => assert_total (idris_crash "Enum")

eventKind : PrimEvent -> EventKind
eventKind ev =
  case prim_eventKind ev of
    0 => KMouseHold
    1 => KMouseRelease
    2 => KMouseClick
    _ => assert_total (idris_crash "Enum")

mouseEvent : PrimEvent -> MouseEvent
mouseEvent ev =
  MkMouse (case prim_mouseEventButton ev of
             0 => BLeft
             1 => BMiddle
             2 => BRight
             _ => assert_total (idris_crash "Enum")) (MkV2 (prim_mouseEventX ev) (prim_mouseEventY ev))

toEvent : PrimEvent -> Maybe Event
toEvent ev =
  if not (hasEvent ev)
    then Nothing
    else
      case eventKind ev of
        KMouseHold => Just (MouseHold (mouseEvent ev))
        KMouseRelease => Just (MouseRelease (mouseEvent ev))
        KMouseClick => Just (MouseClick (mouseEvent ev))

%foreign "browser:lambda:pollEvent"
prim_pollEvent : Window -> PrimIO PrimEvent

public export
pollEvent : Window -> IO (Maybe Event)
pollEvent win = primIO (prim_pollEvent win) >>= pure . toEvent

