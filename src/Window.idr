module Window

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
interface IsObject a where
  blit : Window -> a -> IO ()

public export
data AnyObject : Type where
  MkAnyObj : IsObject a => a -> AnyObject

%foreign "browser:lambda:beginDraw"
prim_beginDraw : Window -> PrimIO ()

public export
beginDraw : Window -> IO ()
beginDraw = primIO . prim_beginDraw

%foreign "browser:lambda:endDraw"
prim_endDraw : Window -> PrimIO ()

public export
endDraw : Window -> IO ()
endDraw = primIO . prim_endDraw

%foreign "browser:lambda:toFrame"
prim_toFrame : IO a -> PrimIO a

public export
frame : IO a -> IO a
frame f = primIO (prim_toFrame f)

