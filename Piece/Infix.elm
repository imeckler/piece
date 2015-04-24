module Piece.Infix
  ( (<>)
  , (+>)
  , (>+>)
  ) where
{-| Infix synonyms. 
# Operators
@docs (<>), (+>), (>+>)
-}

import Piece exposing (..)

{-| (<>) = Piece.followedBy -}
(<>) : Piece ForATime a -> Piece t a -> Piece t a
(<>) = followedBy

infixl 9 <>

{-| (+>) = Piece.chainTo -}
(+>) : Piece ForATime a -> (a -> Piece t a) -> Piece t a
(+>) = chainTo

{-| (>+>) f g = \x -> f x +> g -}
(>+>) : (a -> Piece ForATime a) -> (a -> Piece t a) -> (a -> Piece t a)
(>+>) f g = \x -> f x +> g

infixl 9 +>
