module Component

import Data.List
import Window
import Event

export
data View : Type where
  Obj : IsObject a => a -> View
  Empty : View
  Join : (left : View) -> (right : View) -> View
  Layer : (n : Int) -> View -> View

public export
interface ToView a where
  toView : a -> View

public export
IsObject a => ToView a where
  toView = Obj

public export
ToView View where
  toView = id

export
combineView : (ToView l, ToView r) => l -> r -> View
combineView l r = Join (toView l) (toView r)

export
unitView : View
unitView = Empty

export
layer : ToView v => Int -> v -> View
layer n vw = Layer n (toView vw)

normalise' : Int -> View -> List (Int, AnyObject)
normalise' _ Empty = []
normalise' n (Obj o) = [(n, MkAnyObj o)]
normalise' n (Join l r) = normalise' n l ++ normalise' n r
normalise' _ (Layer n obj) = normalise' n obj

normalise : View -> List AnyObject
normalise obj = map snd $ sortBy (\(a, _), (b, _) => compare a b) $ normalise' 0 obj

export
renderView : Window -> View -> IO ()
renderView win vw = beginDraw win *> traverse_ (\(MkAnyObj o) => blit win o) (normalise vw) <* endDraw win

export
data ModelType : Type where
  MSingleton : Type -> ModelType
  MProduct : Type -> ModelType -> ModelType

export
data Model : ModelType -> Type where
  SingletonModel : t -> Model (MSingleton t)
  ProdModel : t -> Model ts -> Model (MProduct t ts)

public export total
ModelData : ModelType -> Type
ModelData (MSingleton a) = a
ModelData (MProduct a b) = (a, ModelData b)

public export
getData : Model m -> ModelData m
getData (SingletonModel x) = x
getData (ProdModel x y) = (x, getData y)

public export total
Model1 : Type -> ModelType
Model1 t = MSingleton t

public export
model1 : a -> Model (Model1 a)
model1 = SingletonModel

public export
view1 : (a -> View) -> (Model (Model1 a) -> View)
view1 vw = \mdl => vw (getData mdl)

public export total
ProductModel : ModelType -> ModelType -> ModelType
ProductModel (MSingleton t) m2 = MProduct t m2
ProductModel (MProduct t ts) m2 = MProduct t (ProductModel ts m2)

export
combineModel : Model m1 -> Model m2 -> Model (ProductModel m1 m2)
combineModel (SingletonModel x) y = ProdModel x y
combineModel (ProdModel t x) y = ProdModel t (combineModel x y)

export
splitModel : {auto m1 : ModelType} -> Model (ProductModel m1 m2) -> (Model m1, Model m2)
splitModel {m1 = MSingleton t} (ProdModel v r) = (SingletonModel v, r)
splitModel {m1 = MProduct t m} (ProdModel v r) =
  let (l, r) = splitModel {m1 = m} r
  in (ProdModel v l, r)

export
data MessageType : Type where
  SSingleton : Type -> MessageType
  SUnion : Type -> MessageType -> MessageType

export
data Message : MessageType -> Type where
  Singleton : a -> Message (SSingleton a)
  LeftMsg : a -> Message (SUnion a b)
  RightMsg : Message b -> Message (SUnion a b)

public export total
Message1 : Type -> MessageType
Message1 t = SSingleton t

public export
message1 : a -> Message (Message1 a)
message1 = Singleton

public export total
SumMessage : MessageType -> MessageType -> MessageType
SumMessage (SSingleton t) y = SUnion t y
SumMessage (SUnion t x) y = SUnion t (SumMessage x y)

combineMessage : Message s1 -> Message s2 -> Message (SumMessage s1 s2)
combineMessage (Singleton t) y = LeftMsg t
combineMessage (LeftMsg t) y = LeftMsg t
combineMessage (RightMsg x) y = RightMsg (combineMessage x y)

public export total
MessageVariant : MessageType -> Type
MessageVariant (SSingleton t) = t
MessageVariant (SUnion a b) = Either a (MessageVariant b)

public export
getMsg : Message s -> MessageVariant s
getMsg (Singleton x) = x
getMsg (LeftMsg x) = Left x
getMsg (RightMsg y) = Right (getMsg y)

public export
data Elem : Type -> MessageType -> Type where
  HSingleton : Elem a (SSingleton a)
  Here : Elem a (SUnion a s)
  There : Elem a s -> Elem a (SUnion b s)

export
splitElem :
  {s1 : MessageType} ->
  Elem a (SumMessage s1 s2) ->
  Either (Elem a s1) (Elem a s2)

splitElem {s1 = SSingleton a} Here = Left HSingleton
splitElem {s1 = SSingleton t} (There x) = Right x

splitElem {s1 = SUnion a x} Here = Left Here
splitElem {s1 = SUnion t x} (There y) with (splitElem {s1 = x} y)
  splitElem {s1 = SUnion t x} (There y) | Left rec  = Left (There rec)
  splitElem {s1 = SUnion t x} (There y) | Right rec = Right rec

export
splitMessage : 
  {auto s1 : MessageType} -> 
  Message (SumMessage s1 s2) -> 
  Either (Message s1) (Message s2)

splitMessage {s1 = SSingleton a} (LeftMsg val) = Left (Singleton val)
splitMessage {s1 = SSingleton a} (RightMsg msg) = Right msg

splitMessage {s1 = SUnion a x} (LeftMsg val) = Left (LeftMsg val)
splitMessage {s1 = SUnion a x} (RightMsg msg) with (splitMessage {s1 = x} msg)
  splitMessage {s1 = SUnion a x} (RightMsg msg) | Left leftMsg  = Left (RightMsg leftMsg)
  splitMessage {s1 = SUnion a x} (RightMsg msg) | Right rightMsg = Right rightMsg

export
liftMessage : {s : MessageType} -> Elem a s -> a -> Message s
liftMessage HSingleton val = Singleton val
liftMessage Here val = LeftMsg val
liftMessage (There x) val = RightMsg (liftMessage x val)

export
data Controller : MessageType -> ModelType -> Type where
  Handle : (Message s -> Model m -> Model m) -> Controller s m

combineController : 
  {m1 : ModelType} -> {s1 : MessageType} ->
  Controller s1 m1 -> Controller s2 m2 -> Controller (SumMessage s1 s2) (ProductModel m1 m2)
combineController (Handle f) (Handle g) = Handle $ \msg, mdl =>
  let (mdl1, mdl2) = splitModel {m1} mdl
  in case splitMessage {s1} msg of
    Left msg1 => combineModel (f msg1 mdl1) mdl2
    Right msg2 => combineModel mdl1 (g msg2 mdl2)

export
controller1 : (a -> b -> b) -> Controller (Message1 a) (Model1 b)
controller1 f = Handle $ \msg, mdl => model1 (f (getMsg msg) (getData mdl))

export
controllerId : Controller (Message1 Unit) m
controllerId = Handle $ \_, mdl => mdl

export
run : Message s -> Model m -> Controller s m -> Model m
run msg mdl (Handle f) = f msg mdl

export
record Component (s : MessageType) (m : ModelType) where
  constructor MkComponent
  model : Model m
  view : Model m -> View
  controller : Controller s m

export
component1 : Model m -> (Model m -> View) -> Controller s m -> Component s m
component1 = MkComponent

public export
infixl 8 <>

public export
infixl 8 ^

namespace ViewJoin
  public export
  (<>) : (ToView l, ToView r) => l -> r -> View
  (<>) = combineView

namespace ModelJoin
  public export
  (<>) : Model m1 -> Model m2 -> Model (ProductModel m1 m2)
  (<>) = combineModel

namespace MessageJoin
  public export
  (<>) : Message s1 -> Message s2 -> Message (SumMessage s1 s2)
  (<>) = combineMessage

namespace ControllerJoin
  public export
  (<>) : {m1 : ModelType} -> {s1 : MessageType} -> Controller s1 m1 -> Controller s2 m2 -> Controller (SumMessage s1 s2) (ProductModel m1 m2)
  (<>) = combineController

namespace ComponentJoin
  public export
  (<>) : {m1 : ModelType} -> {s1 : MessageType} -> Component s1 m1 -> Component s2 m2 -> Component (SumMessage s1 s2) (ProductModel m1 m2)
  (MkComponent m1 v1 c1) <> (MkComponent m2 v2 c2) =
    MkComponent (combineModel m1 m2) (\m => let (l, r) = splitModel m in v1 l <> v2 r) (c1 <> c2)

public export
windowOf : {s : MessageType} -> Component s m -> {auto el : Elem Event s} -> IO ()
windowOf cmp {el} = withWindow $ \win => frame (handleWindow win cmp)
  where
    winEvents : Window -> Component s m -> IO (Component s m)
    winEvents win (MkComponent m v c) =
      pollEvent win >>= \case
        Just ev => winEvents win $ MkComponent (run (liftMessage el ev) m c) v c
        Nothing => pure (MkComponent m v c)

    handleWindow : Window -> Component s m -> IO ()
    handleWindow win cmp =
      winEvents win cmp >>=
       \(cmp'@(MkComponent m v c)) => renderView win (v m) >>
                                      frame (handleWindow win cmp')
