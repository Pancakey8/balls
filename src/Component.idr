module Component

import Data.List
import Data.Either
import Data.Maybe
import Data.Fin
import Window
import Event
import Geometry
import Control.Monad.Identity
import Debug.Trace

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
  
export
record Job (m : Type -> Type) (steps : Nat) (s : Type) (a : Type) where
  constructor MkJob
  index : Fin (S steps)
  prev : s
  step : Fin steps -> s -> m (Maybe s)
  final : s -> a

export
iterate : Monad m => {n : Nat} -> Job m n s a -> m (Either (Job m n s a) a)
iterate (MkJob idx pv step final) =
  case strengthen idx of
    Nothing => pure (Right (final pv))
    Just idx' => do
      res <- step idx' pv
      case res of
        Nothing => pure (Right (final pv))
        Just res' => pure (Left (MkJob (FS idx') res' step final))

export
iterateN : Monad m => (budget : Nat) -> {k : Nat} -> Job m k s a -> m (Either (Job m k s a) a)
iterateN Z job = pure (Left job)
iterateN (S n) job = 
  iterate job >>= \case
    Left nextJob => iterateN n nextJob
    Right v => pure (Right v)

export
read : Monad m => {n : Nat} -> Job m n s a -> a
read job = job.final job.prev

export
fix : Monad m => {n : Nat} -> Job m n s a -> m a
fix job =
  iterate job >>= \case
    Left job' => fix job'
    Right v => pure v

public export
interface Monad m => Iterative (m : Type -> Type) (s : Nat -> Type) (a : Type) | s where
  iterStep : {k : Nat} -> (i : Fin k) -> s k -> m (Maybe (s k))
  project : {k : Nat} -> s k -> a

export
makeJob : {m : Type -> Type} -> {s : Nat -> Type} -> {a : Type} ->
          Iterative m s a => Monad m =>
          {n : Nat} -> (s n) -> Job m n (s n) a
makeJob iter = MkJob FZ iter (iterStep {k=n}) project

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
data SuffixOf : List Type -> List Type -> Type where
  Base : SuffixOf xs xs
  Step : SuffixOf xs ys -> SuffixOf xs (y :: ys)

public export
dropModel : SuffixOf st target -> Model target -> Model st
dropModel Base model = model
dropModel (Step prf) (x :: xs) = dropModel prf xs

public export
rebuildModel : SuffixOf st target -> Model target -> Model st -> Model target
rebuildModel Base _ newSt = newSt
rebuildModel (Step prf) (x :: xs) newSt = x :: rebuildModel prf xs newSt

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
  liftState : {st, targ : List Type} -> {auto pf : SuffixOf st targ} -> arr a b st -> arr a b targ
  liftState' : {suf, st : List Type} -> arr a b st -> arr a b (st ++ suf)

  (***) : arr inp1 out1 st -> arr inp2 out2 st -> arr (inp1, inp2) (out1, out2) st
  f *** g = first f >>> second g

  (&&&) : {st : List Type} -> arr inp out1 st -> arr inp out2 st -> arr inp (out1, out2) st
  x &&& y = liftState' (arrow dup) >>> x *** y

  loop : arr (inp, shr) (out, shr) (shr :: st) -> arr inp out (shr :: st)

public export
interface Arrow arr => ArrowChoice (arr : Type -> Type -> List Type -> Type) where
  left : arr a b st -> arr (Either a c) (Either b c) st
  right : arr a b st -> arr (Either c a) (Either c b) st

  (+++) : arr a b st -> arr c d st -> arr (Either a c) (Either b d) st
  f +++ g = left f >>> right g

  (\|/) : {st : List Type} -> arr a b st -> arr c b st -> arr (Either a c) b st
  f \|/ g = f +++ g >>> liftState' (arrow fromEither)

public export
interface Arrow arr => ArrowPlus (arr : Type -> Type -> List Type -> Type) where
  zeroArrow : arr a (Maybe b) st
  (<++>) : arr a (Maybe b) st -> arr a (Maybe b) st -> arr a (Maybe b) st
  withDefault : arr a (Maybe b) st -> b -> arr a b st

public export
record ControllerT (m : Type -> Type) (inp : Type) (out : Type) (st : List Type) where
  constructor MkControllerT
  runControllerT : inp -> Model st -> m (out, Model st)

export
hoistCtrl : (Monad m, Monad n) => ({a : Type} -> m a -> n a) -> 
            {out : Type} -> {st : List Type} ->
            ControllerT m inp out st -> ControllerT n inp out st
hoistCtrl f (MkControllerT c) = MkControllerT $ \inp, st => 
  f (c inp st)

export
hoist : (Monad m) => {out : Type} -> {st : List Type} ->
        ControllerT Identity inp out st ->
        ControllerT m inp out st
hoist = hoistCtrl (\(Id x) => pure x)

public export
{m : Type -> Type} ->
Monad m => Profunctor (ControllerT m) where
  lmap f (MkControllerT c) = MkControllerT $ \i, st =>
    c (f i) st
  rmap f (MkControllerT c) = MkControllerT $ \i, st => do
    (out, st') <- c i st
    pure (f out, st')

public export
{m : Type -> Type} ->
Monad m => Arrow (ControllerT m) where
  arrow f = MkControllerT $ \i, [] => pure (f i, [])

  (MkControllerT f) >>> (MkControllerT g) = MkControllerT $ \i, st => do
    (interm, st') <- f i st
    g interm st'

  first (MkControllerT f) = MkControllerT $ \(i, inpOther), st => do
    (i', st') <- f i st
    pure ((i', inpOther), st')

  second (MkControllerT f) = MkControllerT $ \(inpOther, i), st => do
    (i', st') <- f i st
    pure ((inpOther, i'), st')

  liftState (MkControllerT f) = MkControllerT $ \i, allSt => do
    let innerSt = dropModel pf allSt
    (out, innerSt') <- f i innerSt
    pure (out, rebuildModel pf allSt innerSt')

  liftState' {st} (MkControllerT f) = MkControllerT $ \i, allSt => do
    let (st1, st2) = splitModel st allSt
    (out, st1') <- f i st1
    pure (out, st1' ++ st2)

  loop (MkControllerT f) = MkControllerT $ \inp, (prev :: s) => do
    ((out, next), (_ :: s')) <- f (inp, prev) (prev :: s)
    pure (out, next :: s')


public export
{m : Type -> Type} ->
Monad m => ArrowChoice (ControllerT m) where
  left (MkControllerT f) = MkControllerT $ \i, st =>
    case i of
      Left i => do
        (i', st') <- f i st
        pure (Left i', st')
      Right i => pure (Right i, st)

  right (MkControllerT f) = MkControllerT $ \i, st =>
    case i of
      Right i => do
        (i', st') <- f i st
        pure (Right i', st')
      Left i => pure (Left i, st)

public export
{m : Type -> Type} ->
Monad m => ArrowPlus (ControllerT m) where
  zeroArrow = MkControllerT $ \inp, st => pure (Nothing, st)

  (MkControllerT f) <++> (MkControllerT g) = MkControllerT $ \inp, st =>
    f inp st >>= \case
      (Just out, st') => pure (Just out, st')
      (Nothing, _) => g inp st

  withDefault c def = rmap (fromMaybe def) c

public export
record ComponentT (m : Type -> Type) (inp : Type) (out : Type) (st : List Type) where
  constructor MkComponentT
  viewOf : Model st -> View
  ctrlOf : ControllerT m inp out st

public export
{m : Type -> Type} -> Monad m => Profunctor (ComponentT m) where
  lmap f (MkComponentT v c) = MkComponentT v (lmap f c)
  rmap f (MkComponentT v c) = MkComponentT v (rmap f c)

public export
{m : Type -> Type} -> Monad m => Arrow (ComponentT m) where
  arrow f = MkComponentT (\_ => VEmpty) (arrow f)

  (MkComponentT v1 c1) >>> (MkComponentT v2 c2) = 
    MkComponentT (\st => v1 st <> v2 st) (c1 >>> c2)

  first (MkComponentT v c) = 
    MkComponentT v (first c)

  second (MkComponentT v c) = 
    MkComponentT v (second c)

  liftState (MkComponentT v c) = 
    MkComponentT (\allSt => v (dropModel pf allSt)) (liftState c)

  liftState' {st} (MkComponentT v c) = 
    MkComponentT (\allSt => v (fst (splitModel st allSt))) (liftState' c)

  loop (MkComponentT v c) = 
    MkComponentT v (loop c)

public export
Controller : Type -> Type -> List Type -> Type
Controller = ControllerT Identity

public export
Component : Type -> Type -> List Type -> Type
Component = ComponentT Identity

public export
windowOf : {st : List Type} -> ComponentT IO Event output st -> Model st -> IO ()
windowOf cmp initialSt = withWindow $ \win => handleWindow win initialSt
  where
    winEvents : Window -> Model st -> IO (Model st)
    winEvents win currentState = do
      pollEvent win >>= \case
        Just ev => do
          (_, nextState) <- runControllerT cmp.ctrlOf ev currentState
          winEvents win nextState
        Nothing => 
          pure currentState

    handleWindow : Window -> Model st -> IO ()
    handleWindow win state = do
      tick <- windowTick win
      (_, state) <- runControllerT cmp.ctrlOf (GameTick tick) state
      state <- winEvents win state
      let currentView = cmp.viewOf state
      renderView win currentView
      frame (handleWindow win state)
