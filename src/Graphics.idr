module Graphics

import Data.List
import Data.So

export
data Window : Type where [external]

%foreign "browser:lambda:openWindow"
prim_openWindow : PrimIO Window

%foreign "browser:lambda:closeWindow"
prim_closeWindow : Window -> PrimIO ()

export
withWindow : (Window -> IO a) -> IO a
withWindow f = do
  win <- primIO prim_openWindow
  v <- f win
  primIO $ prim_closeWindow win
  pure v

public export
record Vec2 where
  constructor MkV2
  x, y : Double

data Vec2P : Type where [external]

%foreign "browser:lambda:mkVec2"
prim_mkVec2 : (x : Double) -> (y : Double) -> Vec2P

primVec2 : Vec2 -> Vec2P
primVec2 (MkV2 x y) = prim_mkVec2 x y

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

%foreign "browser:lambda:toFrame"
prim_toFrame : IO a -> PrimIO a

public export
frame : IO a -> IO a
frame f = primIO (prim_toFrame f)

public export
record Color where
  constructor MkCol
  cr, cg, cb, ca : Int

data ColorP : Type where [external]

%foreign "browser:lambda:mkColor"
prim_mkColor : (r : Int) ->
               (g : Int) ->
               (b : Int) ->
               (a : Int) ->
               ColorP

primColor : Color -> ColorP
primColor (MkCol r g b a) = prim_mkColor r g b a

public export
interface IsObject a where
  blit : Window -> a -> IO ()

export
data Object : Type where
  Obj : IsObject a => a -> Object
  Empty : Object
  Join : (left : Object) -> (right : Object) -> Object
  Layer : (n : Int) -> Object -> Object

public export
record Circle where
  constructor MkCircle
  center : Vec2
  rad : Double
  color : Color

%foreign "browser:lambda:drawCircle"
prim_drawCircle : (win : Window) -> (center : Vec2P) -> (color : ColorP) -> (rad : Double) -> PrimIO ()

public export
IsObject Circle where
  blit win (MkCircle c r col) = primIO (prim_drawCircle win (primVec2 c) (primColor col) r)

public export
record Fill where
  constructor MkFill
  color : Color

%foreign "browser:lambda:fillWindow"
prim_fillWindow : Window -> ColorP -> PrimIO ()

public export
IsObject Fill where
  blit win (MkFill col) = primIO (prim_fillWindow win (primColor col))

public export
interface ToObject a where
  toObj : a -> Object

public export
IsObject a => ToObject a where
  toObj = Obj

public export
ToObject Object where
  toObj = id

public export
infixl 8 <>

public export
(<>) : (ToObject l, ToObject r) => l -> r -> Object
(<>) left right = Join (toObj left) (toObj right)

public export
unit : Object
unit = Empty

public export
layer : ToObject o => Int -> o -> Object
layer n obj = Layer n (toObj obj)

data AnyObject : Type where
  MkAny : IsObject a => a -> AnyObject

normalise' : Int -> Object -> List (Int, AnyObject)
normalise' _ Empty = []
normalise' n (Obj o) = [(n, MkAny o)]
normalise' n (Join l r) = normalise' n l ++ normalise' n r
normalise' _ (Layer n obj) = normalise' n obj

normalise : Object -> List AnyObject
normalise obj = map snd $ sortBy (\(a, _), (b, _) => compare a b) $ normalise' 0 obj

%foreign "browser:lambda:beginDraw"
prim_beginDraw : Window -> PrimIO ()

%foreign "browser:lambda:endDraw"
prim_endDraw : Window -> PrimIO ()

public export
renderObject : Window -> Object -> IO ()
renderObject win obj = primIO (prim_beginDraw win) *> traverse_ (\(MkAny o) => blit win o) (normalise obj) <* primIO (prim_endDraw win)
