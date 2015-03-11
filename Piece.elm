module Piece
  ( Piece
  , ForATime
  , Forever
  , forever
  , for
  , stayFor
  , stayForever
  , sustain
  , dilate
  , cycle
  , map
  , followedBy
  , chainTo
  , run
  , finalValue
  , layer
  ) where

{-| A type and functions on it for building up values as a function of time.

A `Piece t a` is essentially a function `f : Time -> a` with
an associated duration `dur` which indicates that `f` is
meant to be considered as a function on the
time interval [0, dur].

The first type paramater `t` is `ForATime` if that duration
is some finite time and is `Forever` if `f` is to be considered
a total function.

As an example, we might have a value 
`circlePos : Piece ForATime (Float, Float)` indicating the position
of a circle in an animation.

# Formation
@docs Piece, ForATime, Forever

# Introduction
@docs forever, for, stayFor, stayForever

# Transformation
@docs dilate, cycle, sustain, map

# Composition
@docs followedBy, chainTo

# Elimination
@docs finalValue, run

-}

import Piece.Internal as I
import Queue
import Queue.Internal(..)
import Signal
import Time(Time)
import Time
import List
import List ((::))
import Debug (crash)

modFloat : Float -> Float -> Float
modFloat x m = x - m * toFloat (floor (x / m))

type alias Piece t a = I.Piece t a

{-| A tag type indicating that a `Piece` is defined everywhere -}
type Forever = ForeverDummy
{-| A tag type indicating that a `Piece` is defined on some bounded interval. -}
type ForATime = ForATimeDummy

{-| Create a total `Piece`, defined on all times. -}
forever : (Time -> a) -> Piece Forever a
forever f = I.Piece I.Forever f

{-| Create a `Piece` which runs for the specified duration -}
for : Time -> (Time -> a) -> Piece ForATime a
for dur f = I.Piece (I.ForATime dur) f

{-| A constant `Piece`.

    stayFor dur x = for dur (\_ -> x)
-}
stayFor : Time -> a -> Piece ForATime a
stayFor dur x = for dur (\_ -> x)

{-| An eternally constant `Piece` 

    stayForever x = forever (\_ -> x)
-}
stayForever : a -> Piece Forever a
stayForever x = forever (\_ -> x)

{-| Get the last value the `Piece` takes on. -}
finalValue : Piece ForATime a -> a
finalValue (I.Piece (I.ForATime d) f) = f d

{-| Speed up or slow down time by the given factor. E.g.,

    dilate 0.5 s

is a `Piece` which lasts twice as long as `s` and proceeds half
as fast -}
dilate : Float -> Piece t a -> Piece t a
dilate s (I.Piece dur f) =
  let dur' =
    if s == 0
    then I.Forever
    else case dur of { I.ForATime t -> I.ForATime (t / s); _ -> dur }
  in I.Piece dur' (\t -> f (s * t))

{-| Repeat the given `Piece` forever. -}
cycle : Piece ForATime a -> Piece Forever a
cycle (I.Piece dur f) =
  case dur of
    I.ForATime d -> I.Piece I.Forever (\t -> f (modFloat t d))
    _            -> crash "The impossible happened: Piece.cycle"

map : (a -> b) -> Piece t a -> Piece t b
map g (I.Piece dur f) = I.Piece dur (g << f)

sustain : Piece ForATime a -> Piece Forever a
sustain st = st `followedBy` stayForever (finalValue st)

mkF d1 f1 f2 =
  \t -> if t <= d1 then f1 t else f2 (t - d1)

{-| Sequence two Piece. Thinking of Pieces as functions of
time, s1 `followedBy` s2 is a piecewise function which acts like
`s1` for the duration of s1 and afterwards acts like `s2` (shifted
by the duration of `s1`). For example, we could write

    for (3 * second) (\_ -> "hi") `followedBy` for (1 * second) (\_ -> "bye")

which corresponds to a function of time which is `"hi"` for times in
the interval `[0, 3]` and `"bye"` in the interval `(3, 4]`. Consider 
using the synonym `(<>)` from `Piece.Infix`.
-}
followedBy : Piece ForATime a -> Piece t a -> Piece t a
followedBy (I.Piece dur1 f1) (I.Piece dur2 f2) =
  case (dur1, dur2) of
    (I.ForATime d1, I.Forever)     -> I.Piece I.Forever (mkF d1 f1 f2)
    (I.ForATime d1, I.ForATime d2) -> I.Piece (I.ForATime (d1 + d2)) (mkF d1 f1 f2)
    (I.Forever, _)                 -> crash "The impossible happened: Piece.followedBy"

{-| Create a sequence of two Pieces, giving the second access to the
final value of the first. For example,

    pos = for (3 * second) (\t -> t * (100 / 1*second))
          `chainTo` \finalPos -> for (1*second) (\t -> finalPos - t * (finalPos / 1*second))

Consider using the synonym `(+>)` from `Piece.Infix`.
-}
chainTo : Piece ForATime a -> (a -> Piece t a) -> Piece t a
chainTo (I.Piece dur f) g =
  case dur of
    I.ForATime d -> I.Piece dur f `followedBy` g (f d)
    _          -> crash "The impossible happened: Piece.chainTo"

{-| Convert a Signal of `Piece`s into a `Signal` by sampling using the given
`Signal Time`. -}
run : Signal Time -> Signal (Piece Forever a) -> Signal a
run ts s =
  Signal.map2 (\(t0, I.Piece _ f) t -> f (t - t0)) (Signal.map2 (,) (Signal.sampleOn s ts) s) ts

type EntToEndUpdate a = CTime Time | CPiece (Piece ForATime a)

{-| Create a signal by chaining together Pieces end-to-end.
    The first argument (perhaps should be a -> Piece Forever a) -}
endToEnd : Piece Forever a -> Signal Time -> Signal (Piece ForATime a) -> Signal a
endToEnd (I.Piece _ gap) =
  let update u (x, t0, s, pieces) =
        let (I.Piece (I.ForATime d) f) = s in
        case u of
          CTime t -> if t - t0 < d then (f t, t0, s, pieces) else case Queue.pop pieces of
            Nothing            -> (gap t, t0, s, Queue.empty)
            Just (s', pieces') -> let (I.Piece _ g) = s' in (g t, t, s', pieces')
          CPiece s' -> (x, t0, s, Queue.push s' pieces) -- should really filter out this event
  in
  \ts ss ->
    Signal.foldp update (gap 0, 0, for 0 gap, Queue.empty) {-dummy args-}
      (Signal.merge (Signal.map CPiece ss) (Signal.map CTime ts))
    |> Signal.map (\(x,_,_,_) -> x)

duration : Piece ForATime a -> Time
duration (I.Piece (I.ForATime d) _) = d

type TwoThings a b
  = TheOneThing a
  | TheOtherThing b
-- layered old to new
layer : Signal Time -> Signal (Piece ForATime a) -> Signal (List a)
layer tSig pSig =
  let update u (ps, tCurr, _) = case u of
        TheOneThing t     -> (List.filter (\(tp, p) -> (t - tp) < duration p) ps, t, True)
        TheOtherThing tp' -> (tp' :: ps, tCurr, False)
  in
  Signal.foldp update ([], 0, False)
    (Signal.merge (Signal.map TheOtherThing (Time.timestamp pSig))
      (Signal.map TheOneThing tSig))
  |> filterMap (\(ps, t, ticked) ->
       if ticked then Just (List.map (\(t0, I.Piece _ f) -> f (t - t0)) ps) else Nothing)
       []

popTil : (a -> Bool) -> Queue a -> Maybe (a, Queue a)
popTil f =
  let go q = case Queue.pop q of
        Just (x, q') -> if f x then Just (x, q') else go q'
        Nothing      -> Nothing
  in
  go

filterMap f x s =
  Signal.map f s
  |> Signal.keepIf (\v -> case v of {Just _ -> True; _ -> False}) (Just x)
  |> Signal.map (\v -> case v of Just a -> a)

