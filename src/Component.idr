module Component

import Data.List
import Data.Either
import Data.Maybe
import Window
import Event
import Geometry

export
infixl 8 <>

public export
data View : Type where
  VEmpty : View
  VObj : IsObject a => a -> View
  VMany : List View -> View
  VLayer : (n : Int) -> View -> View

public export
interface ToView a where
  toView : a -> View

public export
IsObject a => ToView a where
  toView = VObj

public export
ToView View where
  toView = id

export
combineView' : View -> View -> View
combineView' (VMany l) (VMany r) = VMany (l ++ r)
combineView' (VMany l) r = VMany (l ++ [r])
combineView' l (VMany r) = VMany (l :: r)
combineView' l r = VMany [l, r]

export
(<>) : (ToView l, ToView r) => l -> r -> View
l <> r = combineView' (toView l) (toView r)

export
unitView : View
unitView = VEmpty

export
layer : ToView v => Int -> v -> View
layer n vw = VLayer n (toView vw)

normalise' : Int -> View -> List (Int, AnyObject)
normalise' _ VEmpty = []
normalise' n (VObj o) = [(n, MkAnyObj o)]
normalise' n (VMany m) = concatMap (normalise' n) m
normalise' _ (VLayer n obj) = normalise' n obj

export
normalise : View -> List AnyObject
normalise obj = map snd $ sortBy (\(a, _), (b, _) => compare a b) $ normalise' 0 obj

export
renderView : Window -> View -> IO ()
renderView win vw = beginDraw win *> traverse_ (\(MkAnyObj o) => blit win o) (normalise vw) <* endDraw win

public export
data Model : List Type -> Type where
  Nil : Model []
  (::) : (x : a) -> Model xs -> Model (a :: xs)

public export
data Elem : Type -> List Type -> Type where
  Here : Elem x (x :: xs)
  There : Elem x xs -> Elem x (y :: xs)

public export
modelAt : Elem t ts -> Model ts -> t
modelAt Here (x :: xs) = x
modelAt (There p) (x :: xs) = modelAt p xs

public export
(++) : Model xs -> Model ys -> Model (xs ++ ys)
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

public export
splitModel : (xs : List Type) -> Model (xs ++ ys) -> (Model xs, Model ys)
splitModel [] ys = ([], ys)
splitModel (x :: xs) (y :: ys) =
  let (l, r) = splitModel xs ys
  in (y :: l, r)

export infixr 5 <++>
export infixr 3 ***
export infixr 3 &&&
export infixr 2 +++
export infixr 2 \|/
export infixr 1 >>>

public export
interface Profunctor (p : Type -> Type -> List Type -> Type) where
  lmap : (a2 -> a1) -> p a1 b st -> p a2 b st
  rmap : (b1 -> b2) -> p a b1 st -> p a b2 st

public export
interface Arrow (arr : Type -> Type -> List Type -> Type) where
  arrow : (a -> b) -> arr a b []
  (>>>) : arr a b st -> arr b c st -> arr a c st
  first : arr a b st -> arr (a, c) (b, c) st
  second : arr a b st -> arr (c, a) (c, b) st
  liftState : (pre : List Type) -> arr a b st -> arr a b (pre ++ st)
  liftState' : (suf : List Type) -> {st : List Type} -> arr a b st -> arr a b (st ++ suf)

  (***) : arr inp1 out1 st -> arr inp2 out2 st -> arr (inp1, inp2) (out1, out2) st
  f *** g = first f >>> second g

  (&&&) : {st : List Type} -> arr inp out1 st -> arr inp out2 st -> arr inp (out1, out2) st
  x &&& y = liftState' st (arrow dup)  >>> x *** y

  loop : arr (inp, shr) (out, shr) (shr :: st) -> arr inp out (shr :: st)

public export
interface Arrow arr => ArrowChoice (arr : Type -> Type -> List Type -> Type) where
  left : arr a b st -> arr (Either a c) (Either b c) st
  right : arr a b st -> arr (Either c a) (Either c b) st

  (+++) : arr a b st -> arr c d st -> arr (Either a c) (Either b d) st
  f +++ g = left f >>> right g

  (\|/) : {st : List Type} -> arr a b st -> arr c b st -> arr (Either a c) b st
  f \|/ g = f +++ g >>> liftState' st (arrow fromEither)

public export
interface Arrow arr => ArrowPlus (arr : Type -> Type -> List Type -> Type) where
  zeroArrow : arr a (Maybe b) st
  (<++>) : arr a (Maybe b) st -> arr a (Maybe b) st -> arr a (Maybe b) st
  withDefault : arr a (Maybe b) st -> b -> arr a b st

public export
record Controller (inp : Type) (out : Type) (st : List Type) where
  constructor MkController
  runController : inp -> Model st -> (out, Model st)

public export
Profunctor Controller where
  lmap f (MkController c) = MkController $ \i, st =>
    c (f i) st
  rmap f (MkController c) = MkController $ \i, st =>
    let (out, st') = c i st
    in (f out, st')

public export
Arrow Controller where
  arrow f = MkController $ \i, [] => (f i, [])

  (MkController f) >>> (MkController g) = MkController $ \i, st =>
    let (interm, st') = f i st
    in g interm st'

  first (MkController f) = MkController $ \(i, inpOther), st =>
    let (i', st') = f i st
    in ((i', inpOther), st')

  second (MkController f) = MkController $ \(inpOther, i), st =>
    let (i', st') = f i st
    in ((inpOther, i'), st')

  liftState pre (MkController f) = MkController $ \i, allSt =>
    let (st1, st2) = splitModel pre allSt
        (out, st2') = f i st2
    in (out, st1 ++ st2')

  liftState' suf {st} (MkController f) = MkController $ \i, allSt =>
    let (st1, st2) = splitModel st allSt
        (out, st1') = f i st1
    in (out, st1' ++ st2)

  loop (MkController f) = MkController $ \inp, (prev :: s) =>
    let ((out, next), (_ :: s')) = f (inp, prev) (prev :: s)
    in (out, next :: s')

public export
ArrowChoice Controller where
  left (MkController f) = MkController $ \i, st =>
    case i of
      Left i =>
        let (i', st') = f i st
        in (Left i', st')
      Right i => (Right i, st)

  right (MkController f) = MkController $ \i, st =>
    case i of
      Right i =>
        let (i', st') = f i st
        in (Right i', st')
      Left i => (Left i, st)

public export
ArrowPlus Controller where
  zeroArrow = MkController $ \inp, st => (Nothing, st)

  (MkController f) <++> (MkController g) = MkController $ \inp, st =>
    case f inp st of
      (Just out, st') => (Just out, st')
      (Nothing, _) => g inp st

  withDefault c def = rmap (fromMaybe def) c

public export
record Component (inp : Type) (out : Type) (st : List Type) where
  constructor MkComponent
  viewOf : Model st -> View
  ctrlOf : Controller inp out st

public export
Profunctor Component where
  lmap f (MkComponent v c) = MkComponent {
    viewOf = v, 
    ctrlOf = lmap f c }
  rmap f (MkComponent v c) = MkComponent {
    viewOf = v, 
    ctrlOf = rmap f c }

public export
Arrow Component where
  arrow f = MkComponent {
    viewOf = \[] => unitView,
    ctrlOf = arrow f }

  (MkComponent v1 c1) >>> (MkComponent v2 c2) = MkComponent {
    viewOf = \mdl => v1 mdl <> v2 mdl,
    ctrlOf = c1 >>> c2 }

  first (MkComponent v c) = MkComponent {
    viewOf = v,
    ctrlOf = first c }

  second (MkComponent v c) = MkComponent {
    viewOf = v,
    ctrlOf = second c }

  liftState pre (MkComponent v c) = MkComponent {
    viewOf = \mdl =>
      let (l, r) = splitModel pre mdl
      in v r,
    ctrlOf = liftState pre c }

  liftState' suf {st} (MkComponent v c) = MkComponent {
    viewOf = \mdl =>
      let (l, r) = splitModel st mdl
      in v l,
    ctrlOf = liftState' suf c }

  loop (MkComponent v c) = MkComponent {
    viewOf = v, 
    ctrlOf = loop c }

public export
ArrowChoice Component where
  left (MkComponent v c) = MkComponent {
    viewOf = v,
    ctrlOf = left c }

  right (MkComponent v c) = MkComponent {
    viewOf = v,
    ctrlOf = right c }

public export
ArrowPlus Component where
  zeroArrow = MkComponent { viewOf = \_ => unitView, ctrlOf = zeroArrow }

  (MkComponent v1 c1 <++> MkComponent v2 c2) = MkComponent {
    viewOf = \mdl => v1 mdl <> v2 mdl,
    ctrlOf = c1 <++> c2 }

  withDefault c def = rmap (fromMaybe def) c

public export
windowOf : {st : List Type} -> Component Event output st -> Model st -> IO ()
windowOf cmp initialSt = withWindow $ \win => handleWindow win initialSt
  where
    winEvents : Window -> Model st -> IO (Model st)
    winEvents win currentState = do
      pollEvent win >>= \case
        Just ev => do
          let (_, nextState) = runController cmp.ctrlOf ev currentState
          winEvents win nextState
        Nothing => 
          pure currentState

    handleWindow : Window -> Model st -> IO ()
    handleWindow win state = do
      state' <- winEvents win state
      let currentView = cmp.viewOf state'
      renderView win currentView
      frame (handleWindow win state')
