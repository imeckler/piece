module Piece.Internal where

{-| Internal implementation of Piece if you need access to it.
    This is all subject to change. -}

import Time (Time)

type Duration = ForATime Time | Forever

type Piece t a = Piece Duration (Time -> a)

