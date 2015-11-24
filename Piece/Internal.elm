module Piece.Internal where

{-| Internal implementation of Piece if you need access to it.
    This is all subject to change.
    
@docs Duration, Piece

-}

import Time exposing (Time)

{-| -}
type Duration = ForATime Time | Forever

{-| -}
type Piece t a = Piece Duration (Time -> a)

