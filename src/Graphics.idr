module Graphics

import Data.List

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
