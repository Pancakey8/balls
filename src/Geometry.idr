module Geometry

import Window

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

public export
Eq Color where
  (MkCol r1 g1 b1 a1) == (MkCol r2 g2 b2 a2) = (r1, g1, b1, a1) == (r2, g2, b2, a2)

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

